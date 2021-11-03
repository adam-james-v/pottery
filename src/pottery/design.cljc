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

(defn show-triangulation
  [{:keys [tris]}]
  (tools/cider-show
   (for [tri tris]
     (-> (el/polygon tri)
         (tf/style {:fill "none" :stroke "white"})))))

(defn blend-extrude-fn
  [curves z]
  (let [bf #(p/multiblend curves su/ease-in-out-sin %)]
    (fn [u v]
      (vec (concat ((bf v) u) [(* z v)])))))

(defn- lift-curve-to-3D
  [curve]
  (fn [t] (concat (curve t) [0])))

(defn extrude-curve2d
  [curve z]
  (fn [u v]
    (su/v+ (concat (curve u) [0]) [0 0 (* z v)])))

(defn extrude-curve3d
  [curve z]
  (fn [u v]
    (su/v+ (curve u) [0 0 (* z v)])))

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
                  (map (range useg))
                  (->> (map #(vec (drop-last %))))
                  alg/clip-ears
                  :tri-indices
                  (->> (map wrap-idx)))
        tface-idx (* useg vseg)
        idx-offset (fn [tri] (mapv #(+ tface-idx %) tri))
        ttris (-> #(surface (double (/ % useg)) 1)
                  (map (range useg))
                  alg/clip-ears
                  :tri-indices
                  (->> (map idx-offset))
                  (->> (map #(vec (reverse %)))))]
    (fm/polyhedron
     (remove nil? (map surf uvs))
     (concat btris tris ttris)
     #_(-> tris
         (conj (concat (range useg) [0]))
         (conj (reverse (concat (range tface-idx (+ tface-idx useg)) [tface-idx])))))))

(def test-surf
  (let [b (p/circle 200)]
    (volume (extrude-curve b 100) 64)))

(spit "pottery.scad" (scad/write test-surf))

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

(defn rect
  [w h]
  (let [[wh hh] (map #(/ % 2.0) [w h])
        uline (p/line [(- wh) (- hh)] [(+ wh) (- hh)])
        vline (p/line [(- wh) (- hh)] [(- wh) (+ hh)])]
    (fn [u v]
      [(first (uline u))
       (second (vline v))
       0])))
      
(defn open-surface
  [surface seg]
  (let [eps 0.00001
        useg seg
        vseg useg
        ustep (double (/ 1 useg))
        vstep (double (/ 1 vseg))
        uvs (map #(vec (reverse %))
                 (p/rect-grid (inc useg) (inc vseg) ustep vstep))
        trifn (fn [idx]
                (when-not (and (not= 0 idx) (= 0 (int (mod (inc idx) (inc seg)))))
                  [[idx (+ idx (inc useg)) (inc idx)]
                   [(inc idx) (+ 1 idx useg) (+ 2 idx useg)]]))
        tris (drop-last 2 (remove nil? (mapcat trifn (drop-last seg (range (inc (count uvs)))))))]
    (fm/polyhedron (map #(apply surface %) uvs) tris)))

(defn bounding-curve
  [surface]
  {})

(defn extrude-surface
  [surface h]
  {})

(def rect-surface
  (-> (p/line [0 0 10] [100 200 -20])
      #_(p/bezier [[0 0] [120 -160] [100 200]])
      #_(p/fn-offset (p/sinwave 10 17))
      (extrude-curve3d 200)
      (open-surface 20)))

(spit "pottery.scad" (scad/write rect-surface))

(defn surface-normal
  [surface [u v]]
  (let [eps 0.00001
        uvs (->> (p/regular-polygon-pts eps 3)
                 (map #(su/v+ % [u v])))
        pts (map #(apply surface %) uvs)]
    (su/normalize (apply su/normal pts)))) 

(defn offset-surface
  [surf t]
  (fn [u v]
    (let [pt (surf u v)
          n (surface-normal surf [u v])
          v (su/v* n (repeat t))]
      (su/v+ pt v))))

(defn offset-surface-volume
  [surf t seg]
  (let [btris (open-surface surf seg)
        ttris (open-surface (offset-surface surf t) seg)]
    [btris ttris]))

(defn bezier-patch
  [apts bpts cpts dpts]
  (let [curves (map p/bezier [apts bpts cpts dpts])]
    (fn [u v]
      (let [curve (p/bezier (map #(% u) curves))]
        (curve v)))))

(def patch
  (-> (bezier-patch
       [[0 0 0] [100 0 0] [200 0 0] [300 0 0]]
       [[10 100 0] [100 100 250] [200 100 -250] [300 100 -20]]
       [[-120 200 0] [100 200 -350] [200 200 250] [300 200 90]]
       [[0 300 0] [100 300 0] [200 300 0] [300 300 0]])
      (open-surface 60)))

(spit "pottery.scad" (scad/write patch))

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
              (p/simplify 400)
              p/polygon)
        m (-> (p/regular-polygon-pts 150 6)
              (p/fillet-pts 40)
              (p/simplify 400)
              p/polygon)
        t (-> (p/regular-polygon-pts 150 3)
              (p/fillet-pts 40)
              (p/simplify 400)
              p/polygon)]
    (vase [b #_m t] 350 10 20)))

(def b-vase
  (let [h 400
        t 16
        seg 50
        bs (-> (p/regular-polygon-pts 140 6)
               (p/fillet-pts 50)
               (p/simplify 400)
               p/polygon)
        ts (-> (p/circle 160)
               (p/rotate 90)
               (p/fn-offset (p/sinwave 7 60)))
        ibs (p/fn-offset bs (fn [_] t))
        its (-> (p/circle (- 160 t))
                (p/rotate 90)
                (p/fn-offset (fn [_] t)))
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

(spit "pottery.scad" (scad/write b-vase))
