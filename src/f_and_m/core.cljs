(ns ^:figwheel-always f_and_m.core
    (:require [rum :as r]
              [cljs.reader :as reader]
              [clojure.set :refer (intersection)]
              [cljsjs.react]
              [f-and-m.svg-event :as sve]
              [jayq.core :refer ($)]
              )
    (:require-macros [jayq.macros :refer [ready]])
  )

(enable-console-print!)

;;;
;; game constants
;;;
(def scale-n 1)
(def rows 10)
(def max-n (* 10 rows))
(def initial-lefts (for [n (range 1 (inc max-n))] n))  ;; initial content of the left panel
(def initial-rights (for [n (range 1 (inc max-n))] nil))  ;; initial content of the left panel
(def gap 20)
(def dot-radius 10)
(def click-interval 333)

;;;
;; unit conversions
;;;
(defn gridx->svgx [side x]
  (condp = side
    :left (* (- x 0.5) dot-radius 2)
    :right (* (+ x 10.5 -0.5) dot-radius 2)))
(defn gridy->svgy [y] (* (- y 0.5) dot-radius 2))

(defn svgx->gridx [x]
  (let [x' (/ x 2 dot-radius)]
    (if (<= x' 10)
      [:left (+ x' 0.5)]
      [:right (- x' 10)])))      ; inverse gridx->svgx

(defn svgy->gridy [y]
  (let [y' (/ y 2 dot-radius)]
    (+ y' 0.5)))

(defn svg->grid
  "svg to side and grid coords"
  [[x y]]
  (let [[side x'] (svgx->gridx x)
        y' (svgy->gridy y)]
    [side [x' y']]))

(def viewport-width (gridx->svgx :right 10.5))
(def viewport-height (gridx->svgx :left 10.5))

;;;
;; define game state once so it doesn't re-initialise on reload
;;;
(defonce game (atom {:title "Factors and Mulitples Game"
                     :lefts initial-lefts
                     :rights initial-rights}))

(defn el [id] (.getElementById js/document id))


;;;
;; game transforms
;;;
(defn grid->dot
  "game coords to integer game coords"
  [[side [x y]]]
  [side [(.round js/Math x) (.round js/Math y)]])

(defn mouse->dot
  "find dot under mouse/touch point"
  [svg-el event]
  (prn (sve/mouse->svg svg-el event))
  (grid->dot (svg->grid (sve/mouse->svg svg-el event))))

;;;
;; game state updates
;;;
(defn click-on
  "click on a dot"
  [dot]
  (prn (str "click-on " dot))
  )

(defn commit-drag
  "commit a drag"
  [start-dot end-dot]
  (prn (str "commit drag " start-dot " -> " end-dot))
  )


;;;;;;;;;

(r/defc left-grid [lefts]
  [:g
   [:rect {:x (gridy->svgy 0.5)
           :y (gridy->svgy 0.5)
           :width (gridy->svgy 10.5)
           :height (gridy->svgy 10.5)
           :fill "rgba(255,100,100,0.3)"
           :rx dot-radius
           }]
   [:rect {:x (gridy->svgy 11)
           :y (gridy->svgy 0.5)
           :width (gridy->svgy 10.5)
           :height (gridy->svgy 10.5)
           :fill "rgba(100,100,255,0.2)"
           :rx dot-radius
           }]
   (for [x (range 1 11)
         y (range 1 (inc rows))]
     (let [[x' y'] [(gridx->svgx :left x) (gridy->svgy y)]
           n (nth lefts (+ x -1 (* 10 (- y 1))))
           dx (if (< n 10) -3.5 (if (< n 100) -6.5 -10.5))
           dy 4.5]
       [:g {:key [x y]
            :transform (str "translate(" x' "," y' ")")}
        [:circle {:cx 0
                  :cy 0
                  :r dot-radius
                  :fill "rgba(255,100,130,1)"
                  :key "lc"
                  }]
        [:text {:x dx
                :y dy
                :fill "#ffffff"
                :font-size 12
                :key :lt} n]]))])

(r/defc right-grid [rights]
  [:g
   (for [x (range 1 11)
         y (range 1 (inc rows))]
     (let [[x' y'] [(gridx->svgx :right x) (gridy->svgy y)]
           n (nth rights (+ x -1 (* 10 (- y 1))))
           dx (if (< n 10) -3.5 (if (< n 100) -6.5 -10.5))
           dy 4.5]
       [:g {:key [x y]
            :transform (str "translate(" x' "," y' ")")}
        [:circle {:cx 0
                  :cy 0
                  :r dot-radius
                  :fill (if n "rgba(255,100,130,1)" "rgba(160,160,255,0.5)")
                  :key "lc"
                  }]
        [:text {:x dx
                :y dy
                :fill "#ffffff"
                :font-size 12
                :key :lt} n]]))])

(def drag-line (atom nil))

(defn drag-start
  "start dragging a dot"
  [event]
  (.preventDefault event)
  (let [g @game]
    (let [dot (mouse->dot (el "svg") event)]
      (prn (str "drag-start " dot))
      (reset! drag-line [[dot dot] (.now js/Date)])))
  )

(defn drag-move
  "continue dragging a dot"
  [event]
  (.preventDefault event)
  (let [end-dot (mouse->dot (el "svg") event)
        [[start-dot _ :as dl] started-at] @drag-line]
    (when dl
      (reset! drag-line [[start-dot end-dot] started-at]))))

(defn drag-stop
  "handle end of drag. Convert to a tap if not moved"
  [event]
  (.preventDefault event)
  (let [
        end-dot (mouse->dot (el "svg") event)
        [[start-dot _] started-at] @drag-line
        now (.now js/Date)
        ]
    (if (and (= start-dot end-dot) (< (- now started-at) click-interval))
      (click-on end-dot)
      (commit-drag start-dot end-dot)
      )
    (reset! drag-line nil)))


(r/defc svg-panel < r/reactive []
  [:svg {:view-box (str "0 0 " viewport-width " " viewport-height)
         :height "100%"
         :width "100%"
         :id "svg"
         :on-mouse-down drag-start
         :on-mouse-move drag-move
         :on-mouse-up drag-stop
         :on-touch-start drag-start
         :on-touch-move drag-move
         :on-touch-end drag-stop
         }
   [:g
    (left-grid (:lefts (r/react game)))
    (right-grid (:rights (r/react game)))]
   ])

(r/defc debug []
  (let [g @game]
    [:div
     [:p (str "lefts " (:lefts g))]
     [:p (str "rights " (:rights g))]]))

;;;
;; Put the app/game in here
;;;
(r/defc game-container []
  [:div {:class "panel panel-default" :style {:margin "2px"}}
   [:div {:class "panel-heading"}
    [:h3 {:class "panel-title"} "Factors and Multiples"
     [:span {:style {:float "right"}} "Longest Chain 20"]]]
   [:div {:class "panel-body"}
    [:p "Click on a number or drag it into the right hand grid where you can continue to reorder them.
Try to make the longest possible chain where each number is a factor or a multiple of its predecessor.
Each number may be used once only.
Any valid chain is coloured green."]
    (svg-panel)
    (debug)]])


;;;
;; mount main component on html game element
;;;
(r/mount (game-container) (el "game"))


;;
;; optionally do something on game reload
;;
(defn on-js-reload []
  (swap! game update-in [:__figwheel_counter] inc))


(defn set-right [n v]
  (swap! game (fn [g] (assoc g :rights (concat
                                       (subvec (vec (:rights @game)) 0 n)
                                       [v]
                                       (subvec (vec (:rights @game)) (inc n) max-n)
                                       )))))
