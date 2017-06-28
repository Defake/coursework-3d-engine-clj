(ns engine-3d.vectors-math
  (:require [engine-3d.objects])
  (:import [org.apache.commons.math3.geometry.euclidean.threed Vector3D]
           [engine_3d.objects Point]))

(defn to-apache-vector [vect]
  (let [vec-type (class vect)]
    (cond
      (= vec-type engine_3d.objects.Point) (Vector3D. (double-array (vals vect)))
      (= vec-type clojure.lang.PersistentVector) (Vector3D. (double-array vect))
      :else vect)))

(defn normalize-vector [vect]
  (.normalize (to-apache-vector vect)))

(defn cross-product [vec1 vec2]
  (let [v3d1 (to-apache-vector vec1)
        v3d2 (to-apache-vector vec2)]
    (Vector3D/crossProduct v3d1 v3d2)))

(defn dot-product [vec1 vec2]
  (let [v3d1 (to-apache-vector vec1)
        v3d2 (to-apache-vector vec2)]
    (Vector3D/dotProduct v3d1 v3d2)))
