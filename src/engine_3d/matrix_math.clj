(ns engine-3d.matrix-math
  (:require [engine-3d.objects :refer [hpoint-transform]])
  (:import [org.apache.commons.math3.linear Array2DRowRealMatrix MatrixUtils]
           [engine_3d.objects Point HPoint]))

;================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MATRICES OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;================================================================================

(defn- to-real-matrix [matrix]
  (if (= (class matrix) clojure.lang.PersistentVector)
    (Array2DRowRealMatrix. (into-array (map double-array matrix)))
    matrix))

(defn multiply [matrix matrix2]
  (.multiply (to-real-matrix matrix) (to-real-matrix matrix2)))

(defn transform-point [matrix ^Point point]
  (let [pos-matrix [[(:x point)]
                    [(:y point)]
                    [(:z point)]
                    [1]]
        res-matrix (multiply matrix pos-matrix)]
    (HPoint. (.getEntry res-matrix 0 0)
             (.getEntry res-matrix 1 0)
             (.getEntry res-matrix 2 0)
             (.getEntry res-matrix 3 0))))

(defn inverse [matrix]
  (MatrixUtils/inverse (to-real-matrix matrix)))

;================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TRANSFORMATION MATRICES ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;================================================================================

(defn- to-rad [angle-val]
  (* (/ angle-val 180) Math/PI))

(defn- get-rotation-x-matrix [angle]
  (let [a (to-rad angle)]
    [[1 0 0 0]
     [0 (Math/cos a) (- (Math/sin a)) 0]
     [0 (Math/sin a) (Math/cos a) 0]
     [0 0 0 1]]))

(defn- get-rotation-y-matrix [angle]
  (let [a (to-rad angle)]
    [[(Math/cos a) 0 (Math/sin a) 0]
     [0 1 0 0]
     [(- (Math/sin a)) 0 (Math/cos a) 0]
     [0 0 0 1]]))

(defn- get-rotation-z-matrix [angle]
  (let [a (to-rad angle)]
    [[(Math/cos a) (- (Math/sin a)) 0 0]
     [(Math/sin a) (Math/cos a) 0 0]
     [0 0 1 0]
     [0 0 0 1]]))

(defn get-rotation-matrix [^Point angle]
  (-> (get-rotation-x-matrix (:x angle))
      (multiply (get-rotation-y-matrix (:y angle)))
      (multiply (get-rotation-z-matrix (:z angle)))))

(defn get-translation-matrix [^Point by-vector]
  [[1 0 0 (:x by-vector)]
   [0 1 0 (:y by-vector)]
   [0 0 1 (:z by-vector)]
   [0 0 0 1]])

(defn get-scale-matrix [^Point scale-vector]
  [[(:x scale-vector) 0 0 0]
   [0 (:y scale-vector) 0 0]
   [0 0 (:z scale-vector) 0]
   [0 0 0 1]])

(defn transform-point-full [^Point point ^Point obj-coord ^Point obj-rotation]
  (-> (get-translation-matrix obj-coord)
      (multiply (get-rotation-matrix obj-rotation))
      (transform-point point)))
