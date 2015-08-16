(ns myreagent.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [myreagent.plot :as plot]
              [myreagent.ml :as ml])
    (:import goog.History))

;; -------------------------
;; Views

(defn home-page []
  [:canvas {:id "draw" :width 600 :height 600} ])

(defn about-page []
  [:div [:h2 "About myreagent"]
   [:div [:a {:href "#/"} "go to the home page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

;; -------------------------
;; Initialize app
(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app"))
  ; (plot/drawblackline ml/training-data)
  (let [canvas-data (plot/get-canvas-context-from-id "draw")
       ctx (:ctx canvas-data)
       width (:width canvas-data)
       height (:height canvas-data)
       chart (plot/make-chart width height ctx)]

  (plot/clear-canvas ctx width height)
  (plot/draw-red-line (ml/hypothesis-line ml/training-data) chart)
  (plot/draw-all-points ml/training-data chart)
  ))

(defn init! []
  (hook-browser-navigation!)
  (mount-root))

; (defn prod [vec1 vec2]
;   (for ))
