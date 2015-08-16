(ns myreagent.plot
  (:require [schema.core :as s
           :include-macros true ;; cljs only
           ]))

(deftype Chart [width height context])

(defn make-chart [width height ctx]
  (s/validate s/Int width)
  (s/validate s/Int height)
  (s/validate js/CanvasRenderingContext2D ctx)
  (Chart. width height ctx)
  )
;; Draw stuff (and never care about ie ever)
;; There's obviously a ton this lib doesn't do, just adding what
;; I need when I need it.
(def request-animation-frame
  (or js/requestAnimationFrame
      js/webkitRequestAnimationFrame))

(defn get-canvas-context-from-id
  "Gets the drawing context from the id of the canvas element.
   Actual context is in a map with the canvas element and some
   other info."
  [id]
  (let [canvas (.getElementById js/document id)]
    {:canvas canvas
     :width (.-width canvas)
     :height (.-height canvas)
     :ctx (.getContext canvas "2d")}))

(defn to-color [& rgbas]
  (let [csv (apply str (interpose ", " rgbas))]
    (str "rgb(" csv ")")))

(defn dec-to-hex-str [dec]
  (s/validate s/Int dec)
  (.toString (js/parseInt dec) 16))

(defn to-hex-color [& rgbas]
  (let [[red green blue] rgbas]
    (str "#" (dec-to-hex-str red) (dec-to-hex-str green) (dec-to-hex-str blue))))

(defmulti draw-object :type)

(defmethod draw-object :rectangle [{:keys [color pos size]} ctx]
  (let [[x y] pos
        [w h] size]
  (aset ctx "fillStyle" (apply to-color color))
  (.fillRect ctx x y w h)))

(def twopi (* 2 (.-PI js/Math)))

(defmethod draw-object :circle [{:keys [color pos size]} ctx]
  (s/validate js/CanvasRenderingContext2D ctx)
  (s/validate js/String color)
  (let [[x y] pos]
  	(aset ctx "fillStyle" color)
  	(.beginPath ctx)
  	(.arc ctx x y size 0 twopi)
  	(.closePath ctx)
  	(.fill ctx)))

(defn translate-point [point chart]
  "Inverts the rendering origin to the bottom-left (Cartesian coords)"
  (let [[x y] point]
    [x (- chart.height y)]))

(defn draw-point [pos chart]
  (s/validate Chart chart)
  (draw-object {:type :circle :color "#000000" :size 1.5 :pos (translate-point pos chart)} chart.context)
  )

(defn draw-all-points [data chart]
  (s/validate Chart chart)
  (mapv
    (fn [pos]
      (draw-point pos chart))
    data))

(defmethod draw-object :line [{:keys [color width posns]} ctx]
  (s/validate js/CanvasRenderingContext2D ctx)
  (s/validate js/String color)

  (let [[startx starty] (first posns)]
    (.beginPath ctx)
    (.moveTo ctx startx starty)
    (doseq [[x y] (rest posns)]
      (.lineTo ctx x y))
    (.closePath ctx)
    (aset ctx "lineWidth" width)
    (aset ctx "strokeStyle" color)
    (print (aget ctx "strokeStyle"))
    (.stroke ctx)))

(defn draw-line [color data chart]
  (s/validate Chart chart)
  (s/validate js/String color)

  (let [
    adjusted-posns (mapv (fn [pos] (translate-point pos chart)) data)
    ; [startx starty] (first adjusted-posns)
    ctx chart.context
    ]

    (print adjusted-posns)
    ;Move to new origin
    (.beginPath ctx)

    (.moveTo ctx 0 chart.height)

    (let [[last-x last-y]  (last adjusted-posns)]
    (print (str last-x last-y))
      (.lineTo ctx last-x last-y))
    ; (.lineTo ctx 100 0)
    ; (.moveTo ctx startx starty)
    ; (doseq [[x y] adjusted-posns]
    ;   (.lineTo ctx x y))
    ; (.closePath ctx)
    (aset ctx "lineWidth" 1.5)
    (aset ctx "strokeStyle" color)
    (.stroke ctx)

    ))

(defn clear-canvas
  "Clears the canvas"
  [ctx width height]
  ; (.save ctx)
  ; (.setTransform ctx 1 0 0 1 0 0)
  (.clearRect ctx 0 0 width height)
  ; (.restore ctx)
  )

(defn draw-scene
  "Draws a sequence of objects to the screen.
   Object must contain various keys depending on their type.
   {:type :rectangle :color [r g b a] :pos [x y] :size [w h]}
   {:type :circle :color [r g b a] :pos [x y] :size r}
   {:type :line :color [r g b a] :posns [x y ...]}
  Objects are drawn in the order received and the pos coordinate
  specifies the upper left corner."
  [objs {:keys [width height ctx]}]
  (clear-canvas ctx width height)
  ;; clear screen first?
  (doseq [obj objs]
    (draw-object obj ctx)))

(defn draw-black-line [data chart]
  (s/validate Chart chart)
  (draw-line "#000000" data chart))

(defn draw-red-line [data chart]
  (s/validate Chart chart)
  (draw-line "#ff0000" data chart))
