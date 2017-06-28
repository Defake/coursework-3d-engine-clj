(ns engine-3d.graphics
  (:require [engine-3d.objects :as objs]
            [engine-3d.camera :as camera]
            [engine-3d.helpers :refer [distance-between-points]]
            [engine-3d.matrix-math :as matrix])
  (:use [seesaw.core]
        [seesaw.graphics]
        [seesaw.color])
  (:import [engine_3d.objects Point]))

(def stroke-width 5)
(def render-container (ref []))

;================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIGURES DRAWING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;================================================================================

(defn draw-line [graphics p1 p2]
  (draw graphics
        (line (first p1) (second p1) (first p2) (second p2))
        (style :foreground (color 0 0 0)
               :stroke     (stroke :width stroke-width))))

(defn draw-outline [graphics points]
  (dotimes [n (- (count points) 1)]
    (draw-line graphics (nth points n) (nth points (inc n))))
  (draw-line graphics (first points) (last points)))

(defn fill-area [graphics color points]
  (let [spoints (seq points)]
    (draw graphics (apply polygon spoints)
          (style :background color))))

(defn draw-figure [graphics clr points]
  (if-not (nil? clr)
    (fill-area graphics clr points))
  (draw-outline graphics points))

(defn transform-to-screen [hpoint]
  (let [ndc (objs/hpoint-transform hpoint)
        hw (/ 700 2)
        hh (/ 600 2)
        point (Point. (+ (* hw (:x ndc)) (+ (:x hpoint) hw))
                      (+ (* hh (:y ndc)) (+ (:y hpoint) hh))
                      (+ (* (/ (- camera/far camera/near) 2) (:z ndc)) (/ (+ camera/far camera/near) 2)))]
    (if (< 0 (:w hpoint))
      point)))

(defn prepare-object-3d-rendering [object]
  (doseq [side (:sides object)]
    (let [transformed-side-points (mapv #(let [coord (transform-to-screen
                                                       (camera/transform-point
                                                         (matrix/transform-point-full (get (:points object) %)
                                                                                      (:coord object)
                                                                                      (:rotation object))))]
                                              (if-not (nil? coord)
                                                (objs/point-to-vec coord 2)))
                                         side)]
      (if-not (< 0 (count (filter nil? transformed-side-points)))
        (dosync
          (alter render-container
                 conj {:side transformed-side-points
                       :color (:color object)
                       :distance (distance-between-points (objs/point* (reduce #(objs/point+ %1 (matrix/transform-point-full (get (:points object) %2)
                                                                                                                             (:coord object)
                                                                                                                             (:rotation object)))
                                                                                (Point. 0 0 0)
                                                                                side)
                                                                       (/ 1 (count side)))
                                                          (:coord @camera/camera))}))))))

(defn draw-prepared-objects [graphics]
  (let [sorted-sides (sort #(- (:distance %2) (:distance %1)) @render-container)]
    (doseq [side-obj sorted-sides]
      (draw-figure graphics (:color side-obj) (:side side-obj))))
  (dosync (ref-set render-container [])))
