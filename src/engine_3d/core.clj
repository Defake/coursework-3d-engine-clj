(ns engine-3d.core
  (:require [engine-3d.interface :refer [init-engine-window show engine-frame]])
  (:use seesaw.core))

(init-engine-window)
(show)

(def fps 50)

(def render-invoker (agent nil))

(defn render [_]
  (send-off *agent* render)
  (repaint! engine-frame)
  (. Thread (sleep (* 1000 (/ 1 fps)))))

(send-off render-invoker render)


