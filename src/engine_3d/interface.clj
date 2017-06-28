(ns engine-3d.interface
  (:require [engine-3d.graphics :refer :all]
            [engine-3d.objects :as objs]
            [engine-3d.camera :as camera])
  (:use [seesaw core mig font chooser]
        [seesaw.graphics]
        [seesaw.color]
        [seesaw.dev])
  (:import [engine_3d.objects Point]))

(defn create-button [btn-name action]
  (action :name btn-name
          :handler action))

(defn text-style [color size]
  (style :foreground color
         :font (str "ARIAL-BOLD-" size)))

;================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MENU ACTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;================================================================================

(defn spin-dialog []
  (let [dialog-result (input "Set spin value")]
    (Double. (clojure.string/replace dialog-result "," "."))))

(defn open-display-options-dlg [fig-type]
  (let [dialog (apply dialog
                      (concat [:option-type :ok-cancel
                               :success-fn (fn [pane]
                                             (let [text-x (.getDocument (select (to-frame pane) [:#tf-x]))
                                                   text-y (.getDocument(select (to-frame pane) [:#tf-y]))
                                                   text-z (.getDocument(select (to-frame pane) [:#tf-z]))
                                                   text-angle-x (select (to-frame pane) [:#angle-x])
                                                   text-angle-y (select (to-frame pane) [:#angle-y])
                                                   text-angle-z (select (to-frame pane) [:#angle-z])
                                                   text-size (.getDocument(select (to-frame pane) [:#tf-size]))
                                                   color-text (.getDocument (select (to-frame pane) [:#tf-color]))]
                                               (invoke-later
                                                 (objs/space-add-object
                                                   (objs/create-object-3d
                                                     fig-type
                                                     (Point. (Double. (clojure.string/replace (.getText text-x 0 (.getLength text-x)) "," "."))
                                                             (Double. (clojure.string/replace (.getText text-y 0 (.getLength text-y)) "," "."))
                                                             (Double. (clojure.string/replace (.getText text-z 0 (.getLength text-z)) "," ".")))
                                                     (Point. (selection text-angle-x)
                                                             (selection text-angle-y)
                                                             (selection text-angle-z))
                                                     (let [clr-txt (.getText color-text 0 (.getLength color-text))]
                                                      (if (= clr-txt "")
                                                        nil
                                                        (color (str "#" clr-txt))))
                                                     (Double. (clojure.string/replace (.getText text-size 0 (.getLength text-size)) "," "."))
                                                     0)))))]
                              [:content (mig-panel :items [[(label :font (font :from (default-font "Label.font")
                                                                               :style :bold)
                                                                   :text "Set new object parameters")
                                                            "gaptop 5, wrap"]
                                                           [:separator "growx, wrap, gaptop 10, spanx"]
                                                           ["Color" "spanx 1"]
                                                           ["#" "spanx 1"]
                                                           [(text :id :tf-color :text "+FFFFFF" :margin [0 1 0 1]) "wrap, spanx"]
                                                           ["Angle-X"]
                                                           [(slider :id :angle-x :min 0 :max 360 :value 0 :minor-tick-spacing 20 :major-tick-spacing 60 :paint-labels? true)
                                                            "wrap, spanx"]
                                                           ["Angle-Y"]
                                                           [(slider :id :angle-y :min 0 :max 360 :value 0 :minor-tick-spacing 20 :major-tick-spacing 60 :paint-labels? true)
                                                            "wrap, spanx"]
                                                           ["Angle-Z"]
                                                           [(slider :id :angle-z :min 0 :max 360 :value 0 :minor-tick-spacing 20 :major-tick-spacing 60 :paint-labels? true)
                                                            "wrap, spanx"]
                                                           ["Size" "spanx 1"]
                                                           [(text :id :tf-size :text "+00000" :margin [0 1 0 1]) "spanx"]
                                                           ["Coordinates" "spanx 1"]
                                                           ["X" "spanx 1"]
                                                           [(text :id :tf-x :text "+00000" :margin [0 1 0 1]) "spanx 1"]
                                                           ["Y" "spanx 1"]
                                                           [(text :id :tf-y :text "+00000" :margin [0 1 0 1]) "spanx 1"]
                                                           ["Z" "spanx 1"]
                                                           [(text :id :tf-z :text "+00000" :margin [0 1 0 1]) "spanx 1, wrap"]])]))]
    (pack! dialog)
    (invoke-later (text! (select (to-frame dialog) [:#tf-x]) "0"))
    (invoke-later (text! (select (to-frame dialog) [:#tf-y]) "0"))
    (invoke-later (text! (select (to-frame dialog) [:#tf-z]) "0"))
    (invoke-later (text! (select (to-frame dialog) [:#tf-color]) ""))
    (invoke-later (text! (select (to-frame dialog) [:#tf-size]) "100"))
    (show! dialog)))



;================================================================================
;;;;;;;;;;;;;;;;;;;;;; CREATING ENGINE WINDOW INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;
;================================================================================

(def engine-frame
  (frame
    :title "3D Engine"
    :width 700 :height 600))

(listen engine-frame
        :focus-gained (fn [e])
        :focus-lost (fn [e])
        :key-pressed (fn [^java.awt.event.KeyListener e]
                       (case (.getKeyCode e)
                         87 (camera/camera-control :forward true)
                         83 (camera/camera-control :backward true)
                         40 (camera/camera-control :down true)
                         38 (camera/camera-control :up true)
                         65 (camera/camera-control :left true)
                         68 (camera/camera-control :right true)
                         37 (camera/camera-control :r-left true)
                         39 (camera/camera-control :r-right true)
                         82 (camera/reset-camera)
                         nil))
        :key-released (fn [^java.awt.event.KeyListener e]
                        (case (.getKeyCode e)
                          87 (camera/camera-control :forward false)
                          83 (camera/camera-control :backward false)
                          65 (camera/camera-control :left false)
                          68 (camera/camera-control :right false)
                          40 (camera/camera-control :down false)
                          38 (camera/camera-control :up false)

                          37 (camera/camera-control :r-left false)
                          39 (camera/camera-control :r-right false)
                          nil)))

(defn display [object]
  (config! engine-frame :content object))

(config! engine-frame
         :menubar (menubar :items
                           [(menu :text "Create" :items [(action :name "Cube"
                                                                 :handler (fn [_] (open-display-options-dlg :cube)))
                                                         (action :name "Tetrahedron"
                                                                 :handler (fn [_] (open-display-options-dlg :tetra-n)))])
                            (menu :text "Parameters" :items [(checkbox-menu-item :text "Gravity"
                                                                                 :listen [:action #(objs/set-gravity (selection %))])
                                                             (action :name "Set spin"
                                                                     :handler (fn [_] (objs/set-spin (spin-dialog))))
                                                             (action :name "Add standart figures"
                                                                     :handler (fn [_] (objs/add-start-figures)))])]))


(defn show []
  (show! engine-frame))

(defn paint [canvas graphics]
  (let [w (.getWidth canvas)
        h (.getHeight canvas)]
    (doseq [atm @objs/space-3d
            :let [obj @atm]]
      (prepare-object-3d-rendering obj))
    (draw-prepared-objects graphics)
    (let [coords (:coord @camera/camera)
          rot (:rotation @camera/camera)]
      (draw graphics (string-shape 10 25 (str "X: " (:x coords) "  Y: " (:y coords) "  Z: " (:z coords))) (text-style "#000" 16))
      (draw graphics (string-shape 10 46 (str "X: " (:x rot) "  Y: " (:y rot) "  Z: " (:z rot))) (text-style "#000" 16)))))


(defn init-engine-window []
  (display
    (canvas :id :canvas :background "#EED" :paint paint)))
