(ns engine-3d.helpers
  (:require [engine-3d.objects])
  (:import [engine_3d.objects Point]))

(defn distance-between-points [^Point p1 ^Point p2]
  (Math/sqrt (+ (Math/pow (- (:x p2) (:x p1)) 2)
                (Math/pow (- (:y p2) (:y p1)) 2)
                (Math/pow (- (:z p2) (:z p1)) 2))))

(defn clamp [value min-val max-val]
  (min (max value min-val) max-val))

(defn clamp-point [^Point point min-val max-val]
  (Point. (clamp (:x point) min-val max-val)
          (clamp (:y point) min-val max-val)
          (clamp (:z point) min-val max-val)))

(defn bool-to-int [bool]
  (if (= bool true)
    1
    0))
