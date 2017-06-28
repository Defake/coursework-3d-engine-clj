(ns engine-3d.camera
  (:require [engine-3d.objects :as objs]
            [engine-3d.matrix-math :as matrix]
            [engine-3d.vectors-math :as vmath]
            [engine-3d.helpers :refer [bool-to-int clamp-point]])
  (:import [engine_3d.objects Point]))

(defrecord Camera [^Point coord
                   ^Point rotation])

(defn create-camera-3d [^Point coord ^Point rotation]
  (Camera. coord rotation))

(def camera (atom (create-camera-3d
                    (Point. 0 0 800)
                    (Point. 0 0 0))))

(defn reset-camera []
  (swap! camera (fn [_]
                  (create-camera-3d
                    (Point. 0 0 800)
                    (Point. 0 0 0)))))

;================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CAMERA CONTROLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;================================================================================

(def moving-up (atom false))
(def moving-down (atom false))
(def moving-forward (atom false))
(def moving-backward (atom false))
(def moving-left (atom false))
(def moving-right (atom false))
(def rotating-up (atom false))
(def rotating-down (atom false))
(def rotating-left (atom false))
(def rotating-right (atom false))

(defmulti camera-control (fn [control turn-the-control-on] control))

(defmethod camera-control :up [_ on]
  (swap! moving-up (fn [_] on)))

(defmethod camera-control :down [_ on]
  (swap! moving-down (fn [_] on)))

(defmethod camera-control :forward [_ on]
  (swap! moving-forward (fn [_] on)))

(defmethod camera-control :backward [_ on]
  (swap! moving-backward (fn [_] on)))

(defmethod camera-control :left [_ on]
  (swap! moving-left (fn [_] on)))

(defmethod camera-control :right [_ on]
  (swap! moving-right (fn [_] on)))

(defmethod camera-control :r-up [_ on]
  (swap! rotating-up (fn [_] on)))

(defmethod camera-control :r-down [_ on]
  (swap! rotating-down (fn [_] on)))

(defmethod camera-control :r-left [_ on]
  (swap! rotating-left (fn [_] on)))

(defmethod camera-control :r-right [_ on]
  (swap! rotating-right (fn [_] on)))

(def camera-speed 15)
(def camera-rotating-speed 3)

(defn get-rotated-angle [angle]
  (let [init-rot (:rotation @camera)
        inv-rot (objs/invert-point init-rot)
        rot1-matrix (matrix/get-rotation-matrix init-rot)
        rot2-matrix (matrix/get-rotation-matrix inv-rot)
        origin-point (matrix/transform-point rot2-matrix (objs/point* init-rot (/ 1 90)))
        new-p (objs/point+ origin-point angle)
        rotated-vec (objs/point* (matrix/transform-point rot1-matrix new-p) 90)]
    rotated-vec))

(defn controls-update [cam]
  (let [result-moving (objs/point* (matrix/transform-point (matrix/get-rotation-matrix (:rotation @camera))
                                                           (Point. (- (bool-to-int @moving-left) (bool-to-int @moving-right))
                                                                   (- (bool-to-int @moving-up) (bool-to-int @moving-down))
                                                                   (- (bool-to-int @moving-backward) (bool-to-int @moving-forward))))
                                   camera-speed)
        result-rotating (objs/point* (Point. (- (bool-to-int @rotating-down) (bool-to-int @rotating-up))
                                             (- (bool-to-int @rotating-right) (bool-to-int @rotating-left))
                                             0)
                                     camera-rotating-speed)]
    (if-not (= result-moving result-rotating (Point. 0 0 0))
      (let [transformed-cam (-> cam
                                (objs/move-object-3d result-moving)
                                (objs/rotate-object-3d result-rotating))]
        transformed-cam)
      cam)))

(swap! camera (fn [_] (assoc @camera :rotation (objs/point+ (:rotation @camera) (Point. -1 0 1)))))

;================================================================================
;;;;;;;;;;;;;;;;;;;;;;;; CAMERA COORDINATE TRANSFORMING ;;;;;;;;;;;;;;;;;;;;;;;;;
;================================================================================

(def view-matrix (atom [[1 0 0 0]
                        [0 1 0 0]
                        [0 0 1 0]
                        [0 0 0 1]]))

(defn transform-point [^Point point]
  (matrix/transform-point
     @view-matrix
     point))

(def right 300)
(def left -300)
(def top 300)
(def bottom -300)
(def far 1000)
(def near 10)

(defn persp-matrix []
  (let [fov (/ Math/PI 3.67)
        aspect (/ 7 6)
        xymax (* near (Math/tan (* fov 2 Math/PI)))
        ymin (- xymax)
        xmin (- xymax)
        width (- xymax xmin)
        height (- xymax ymin)
        depth (- far near)
        q (/ (- (+ far near)) depth)
        qn (/ (* -2 (* far near)) depth)
        w (/ (/ (* 2 near) width) aspect)
        h (/ (* 2 near) height)]
    [[w 0 0 0]
     [0 h 0 0]
     [0 0 q qn]
     [0 0 -1 0]]))

(defn update-view-matrix [old-matrix]
  (let [proj-matrix (persp-matrix)
        view-matrix (matrix/inverse (-> (matrix/get-translation-matrix (:coord @camera))
                                        (matrix/multiply (matrix/get-rotation-matrix (:rotation @camera)))))]
    (matrix/multiply proj-matrix view-matrix)))

;================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CAMERA MAINTAIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;================================================================================

(def camera-agent (agent nil))
(def camera-sleep-time 25)

(defn camera-loop [_]
  (send-off *agent* camera-loop)
  (swap! camera controls-update)
  (swap! view-matrix update-view-matrix)
  (. Thread (sleep camera-sleep-time)))

(send-off camera-agent camera-loop)

(deref camera)
(deref view-matrix)



(agent-error camera-agent)

