(ns engine-3d.objects
  (:use [seesaw.color]))

;================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OBJECT PROPERTIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;================================================================================

(defrecord Point [x y z])
(defrecord HPoint [x y z w])

(defn hpoint-transform [point]
  (Point. (/ (:x point) (:w point))
          (/ (:y point) (:w point))
          (/ (:z point) (:w point))))

(defn hpoint-to-point [point]
  (Point. (:x point)
          (:y point)
          (:z point)))

(defn point+ [& points]
  (Point. (apply + (mapv :x points))
          (apply + (mapv :y points))
          (apply + (mapv :z points))))

(defn point* [point by]
  (Point. (* by (:x point))
          (* by (:y point))
          (* by (:z point))))

(defn invert-point [^Point point]
  (Point. (-> point :x -)
          (-> point :y -)
          (-> point :z -)))

(defn point-to-vec [point dimensions]
  (mapv #(nth (vals point) %) (range dimensions)))


;================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 3D OBJECTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;================================================================================

(defrecord Object3D [^Point coord
                     ^Point rotation
                     color
                     points
                     sides
                     gravity])

(defn set-coord-object-3d [object coordinate]
  (assoc object :coord coordinate))

(defn move-object-3d [object coordinate]
  (set-coord-object-3d object (point+ (:coord object) coordinate)))

(defn rotate-object-3d [object ^Point by-angle]
  (assoc object :rotation (point+ (:rotation object) by-angle)))

;================================================================================
;;;;;;;;;;;;;;;;;;;;;;;; PREPARED 3D OBJECTS MODELS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;================================================================================

(defmulti create-object-3d (fn [obj-type ^Point coord ^Point angle color & params] obj-type))

(defmethod create-object-3d :tetra-n [_ coord angle color & [radius gravity]]
  (let [hr (/ radius 2)]
    (Object3D. coord angle color
               [(Point. (- hr) (- hr) (- hr))
                (Point. hr (- hr) (- hr))
                (Point. 0 (- hr) hr)
                (Point. 0 hr 0)]
               [[0 1 3]
                [0 2 3]
                [1 2 3]
                [0 1 2]]
               gravity)))

(defmethod create-object-3d :p-piped [_ coord angle color & [width height depth gravity]]
  (let [hw (/ width 2)
        hh (/ height 2)
        hd (/ depth 2)]
    (Object3D. coord angle color
               [(Point. (- hw) (- hh) (- hd))
                (Point. (- hw) hh (- hd))
                (Point. hw hh (- hd))
                (Point. hw (- hh) (- hd))
                (Point. (- hw) (- hh) hd)
                (Point. (- hw) hh hd)
                (Point. hw hh hd)
                (Point. hw (- hh) hd)]
               [[0 1 2 3]
                [4 5 6 7]
                [0 1 5 4]
                [1 2 6 5]
                [2 3 7 6]
                [0 3 7 4]]
               gravity)))

(defmethod create-object-3d :cube [_ coord angle color & [width gravity]]
  (create-object-3d :p-piped coord angle color width width width gravity))

;================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SPACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;================================================================================

(def space-3d (ref []))

(defn space-add-object [object]
  (dosync
    (alter space-3d conj (atom object)))
  (last @space-3d))

(defn add-start-figures []
  (space-add-object (create-object-3d :tetra-n (Point. -150 150 50) (Point. 0 0 0) (color "#C24") 50 0))
  (space-add-object (create-object-3d :tetra-n (Point. -50 -200 -50) (Point. 0 0 0) (color "#02E") 50 0))
  (space-add-object (create-object-3d :tetra-n (Point. -250 0 350) (Point. 0 0 0) (color "#6D8") 80 0))

  (space-add-object (create-object-3d :cube (Point. 10 -40 200) (Point. 0 0 0) (color "#DDD") 100 0))

  (space-add-object (create-object-3d :tetra-n (Point. 200 200 150) (Point. 0 0 0) (color "#4AF") 150 0))
  (space-add-object (create-object-3d :cube (Point. 400 -250 -150) (Point. 0 0 0) (color "#2E2") 120 0)))

(add-start-figures)

(def rotating-agent (agent nil))
(def rotate-value (atom (Point. 0 0.2 0)))
(def g (atom 0))

(defn set-gravity [option]
  (swap! g (fn [g]
             (if (= option true)
               7
               (do
                 (doseq [atm @space-3d]
                   (swap! atm (fn [obj]
                                (if-not (nil? obj)
                                  (assoc obj :gravity 0)))))
                 0)))))

(defn set-spin [value]
  (swap! rotate-value (fn [_] (Point. 0 value 0))))

(defn mod-objects [_]
  (send-off *agent* mod-objects)
  (doseq [atm @space-3d]
    (swap! atm (fn [obj]
                 (if-not (nil? obj)
                   (do
                     (let [modified (assoc obj
                                      :rotation (point+ (:rotation obj) @rotate-value)
                                      :gravity (+ (/ @g 20) (:gravity obj))
                                      :coord (point+ (:coord obj) (Point. 0 (- (:gravity obj)) 0)))]
                       (if (> -5000 (:y (:coord modified)))
                         nil
                         modified)))))))
  (. Thread (sleep 20)))

(send-off rotating-agent mod-objects)

(agent-error rotating-agent)
