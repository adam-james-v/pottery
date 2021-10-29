(ns pottery.design
  (:require [clojure.string :as str]
            [scad-clj.model :as sm]
            [scad-clj.scad :refer [write-scad]]
            [forge.model :as fm]
            [forge.utils :as fu]
            [forge.brep :as brep]
            [forge.import.image :as img]
            [forge.compile.scad :as scad]
            [svg-clj.utils :as su]
            [svg-clj.elements :as el]
            [svg-clj.path :as path]
            [svg-clj.composites :as composites :refer [svg]]
            [svg-clj.transforms :as tf]
            [svg-clj.parametric :as p]
            [svg-clj.algorithms :as alg]
            [svg-clj.tools :as tools]))

(defn shift-pts
  [pts]
  (let [start (first (sort-by (juxt first second) pts))
        [back front] (split-with (complement #{start}) pts)]
    (concat front back)))

(defn simplify
  [pts n]
  (let [c (p/polygon pts)]
    (mapv #(c (/ % (inc n))) (range n))))

(defn show2D
  [sk]
  (let [pts (if (fn? sk)
              (map sk (range 0 1 0.001))
              sk)]
    (-> pts
        el/polyline
        (tf/style {:fill "none"
                   :stroke "white"
                   :stroke-width 2})
        tools/cider-show)))

(defn blend-extrude-fn
  [curves z]
  (let [bf #(p/multiblend curves su/ease-in-out-sin %)]
    (fn [u v]
      (vec (concat ((bf v) u) [(* z v)])))))

(defn- tri-indices
  [seg idx]
  (if (and (not= 0 idx) (= 0 (int (mod (inc idx) seg))))
    ;; when idx is a multiple of seg, loop the surface back to u 0
    [[idx (+ idx seg) (+ 1 idx (- seg))]
     [(+ 1 idx (- seg)) (+ idx seg) (+ 1 idx)]]
    ;; otherwise, use simple idx shifting, assuming the u 1 edge pts are NOT present in the pts list
    [[idx (+ idx seg) (+ 1 idx)]
     [(+ 1 idx) (+ idx seg) (+ 1 idx seg) ]]))

(defn volume
  [surface seg]
  (let [eps 0.00001
        surf (fn [[u v]]
               (when-not (< (- 1 u) eps) (surface u v)))
        useg seg
        vseg useg
        ustep (double (/ 1 useg))
        vstep (double (/ 1 vseg))
        uvs (map #(vec (reverse %))
                 (p/rect-grid (inc useg) (inc vseg) ustep vstep))
        tris (mapcat #(tri-indices seg %) (drop-last seg (range (inc (count uvs)))))
        wrap-idx (fn [tri]
                   (mapv #(if (and (not= 0 %) (= 0 (int (mod % seg))))
                            (- % seg)
                            %)
                         tri)) 
        btris (-> #(surface (double (/ % useg)) 0)
                  (map (range (inc useg)))
                  alg/triangulate
                  :tri-indices
                  (->> (map wrap-idx))
                  (->> (map #(vec (reverse %)))))
        tface-idx (* useg vseg)
        idx-offset (fn [tri] (mapv #(+ tface-idx %) tri))
        ttris (-> #(surface (double (/ % useg)) 1)
                  (map (range (inc useg)))
                  alg/triangulate
                  :tri-indices
                  (->> (map idx-offset)))]
    (fm/polyhedron
     (remove nil? (map surf uvs))
     #_(concat btris tris ttris)
     (-> tris
         (conj (concat (range useg) [0]))
         (conj (reverse (concat (range tface-idx (+ tface-idx useg)) [tface-idx])))))))

(defn- u-closed-surface
  [surface]
  (let [eps 0.00001
        surf (fn [[u v]]
               (when-not (< (- 1 u) eps) (surface u v)))
        seg 64
        useg seg
        vseg useg
        ustep (double (/ 1 useg))
        vstep (double (/ 1 vseg))
        uvs (map #(vec (reverse %))
                 (p/rect-grid (inc useg) (inc vseg) ustep vstep))
        tris (mapcat #(tri-indices seg %) (drop-last seg (range (inc (count uvs)))))]
    (fm/polyhedron (remove nil? (map surf uvs)) tris)))

(defn- open-surface
  [surface]
  (let [eps 0.00001
        seg 64
        useg seg
        vseg useg
        ustep (double (/ 1 useg))
        vstep (double (/ 1 vseg))
        uvs (map #(vec (reverse %))
                 (p/rect-grid (inc useg) (inc vseg) ustep vstep))
        trifn (fn [idx]
                [[idx (+ idx (inc useg)) (inc idx)]
                 [(inc idx) (+ 1 idx useg) (+ 2 idx useg)]])
        tris (mapcat trifn (drop-last seg (range (inc (count uvs)))))]
    (fm/polyhedron (map #(apply surface %) uvs) tris)))

(defn vase
  [shapes h t seg]
  (let [ishapes (map #(p/fn-offset % (fn [_] t)) shapes)
        width (-> #(map % (range 0 1 0.05))
                  (mapcat shapes)
                  su/bb-dims
                  (#(apply max %))
                  (* 2))
        body (volume (blend-extrude-fn shapes h) seg)
        ibody (volume (blend-extrude-fn ishapes (+ h (* h 0.04))) seg)]
    (fm/union
     (fm/difference
      body
      (-> ibody
          (fm/translate [0 0 (* h -0.015)])))
    (fm/difference
       body
       (-> (fm/box width width h)
           (fm/translate [0 0 (+ (* h 0.5) t)]))))))

(def a-vase
  (let [b (-> (p/regular-polygon-pts 150 9)
              (p/fillet-pts 40)
              (simplify 400)
              p/polygon)
        m (-> (p/regular-polygon-pts 150 6)
              (p/fillet-pts 40)
              (simplify 400)
              p/polygon)
        t (-> (p/regular-polygon-pts 150 3)
              (p/fillet-pts 40)
              (simplify 400)
              p/polygon)]
    (vase [b m t] 350 10 50)))

(def b-vase
  (let [h 400
        t 16
        seg 100
        bs (-> (p/regular-polygon-pts 140 6)
               (p/fillet-pts 50)
               (simplify 400)
               p/polygon)
        ts (-> (p/circle 160)
               (p/rotate 90)
               (p/fn-offset (p/sinwave 7 60)))
        ibs (p/fn-offset bs (fn [_] t))
        its (-> (p/circle (- 160 t))
                (p/rotate 90)
                (p/fn-offset (p/sinwave 3 60)))
        width (-> #(map % (range 0 1 0.05))
                  (mapcat [bs ts])
                  su/bb-dims
                  (#(apply max %))
                  (* 2))
        body (volume (blend-extrude-fn [bs ts] h) seg)
        ibody (volume (blend-extrude-fn [ibs its] (+ h (* h 0.04))) seg)]
    (fm/union
     (fm/difference
      body
      (-> ibody
          (fm/translate [0 0 (* h -0.015)])))
    (fm/difference
       body
       (-> (fm/box width width h)
           (fm/translate [0 0 (+ (* h 0.5) t)]))))))

(spit "pottery.scad" (scad/write a-vase))
