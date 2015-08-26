(ns ^figwheel-always f-and-m.svg-event)

(defn touchXY
  "get position of first changed touch"
  [event]
  (let [touch (aget (.-changedTouches event) 0)]
    [(.-clientX touch) (.-clientY touch)]))

(defn mouseXY
  "get mouse position"
  [event]
  [(.-clientX event) (.-clientY event)])

(defn eventXY
  "get touch or mouse position"
  [event]
  (let [type (subs (.-type event) 0 5)]
    (condp = type
      "mouse" (mouseXY event)
      "touch" (touchXY event))))

(def svg-point (atom nil))

(defn mouse->svg
  "browser independent transform from mouse/touch coords to svg viewport"
  [svg event]
  (let [pt (if @svg-point
             @svg-point
             (do
               (reset! svg-point (.createSVGPoint svg))
               @svg-point))
        matrix (.inverse (.getScreenCTM svg))
        [x y] (eventXY event)]
    (aset pt "x" x)
    (aset pt "y" y)
    (reset! svg-point (.matrixTransform pt matrix))
    [(.-x @svg-point) (.-y @svg-point)]))
