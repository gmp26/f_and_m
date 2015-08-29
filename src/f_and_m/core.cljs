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
(def grid-size 10)
(def rows grid-size)
(def cols grid-size)
(def max-n (* cols rows))

(def initial-lefts (vec (for [n (range 1 (inc max-n))] n))) ;; initial content of the left panel
(def initial-rights (vec (for [n (range 1 (inc max-n))] nil))) ;; initial content of the left panel


(def gap 20)
(def dot-radius 10)
(def click-interval 333)

;;;
;; unit conversions
;;;
(defn gridx->svgx [side x]
  (condp = side
    :left (* (+ x 0.5) dot-radius 2)
    :right (* (+ x cols 1) dot-radius 2)))
(defn gridy->svgy [y] (* (+ y 0.5) dot-radius 2))

(defn svgx->gridx [x]
  (let [x' (/ x 2 dot-radius)]
    (if (<= x' 10)
      [:left (- x' 0.5)]
      [:right (- x' cols 0.5)])))      ; inverse gridx->svgx

(defn svgy->gridy [y]
  (let [y' (/ y 2 dot-radius)]
    (- y' 0.5)))

(defn svg->grid
  "svg to side and grid coords"
  [[x y]]
  (let [[side x'] (svgx->gridx x)
        y' (svgy->gridy y)]
    [side [x' y']]))

(def viewport-width (gridx->svgx :right (+ cols 0.5)))
(def viewport-height (gridy->svgy (+ rows 0.5)))

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
(defn sq [x] (* x x))

(defn grid->dot
  "game coords to dot coords. Return nil if not on a dot"
  [[side [x y]]]
  (let [[x' y'] [(.round js/Math x) (.round js/Math y)]]
    (if (<= (+ (sq (- x x')) (sq (- y y'))) 0.25)
      [side [x' y']]
      nil)))

(defn mouse->dot
  "find dot under mouse/touch point, Return nil if not on a dot"
  [svg-el event]
  (grid->dot (svg->grid (sve/mouse->svg svg-el event))))

;;;
;; painting
;;;
(defn xy->index [x y] (+ x (* cols y)))

(defn cell-content
  "dot number"
  [content x y]
  (nth content (xy->index x y))
  )
(def dot-fills {:left {:present "rgba(255,100,130,1)"
                       :absent "rgba(160,170,200,1)"
                       :active  "rgba(255,100,130,1)"}
                :right {:present "#3B53FE" ;"rgba(255,100,130,1)"
                        :chained "rgba(80,220,80,1)"
                        :absent "rgba(160,160,255,0.5)"
                        :displaced "rgba(255,100,130,0.7)"}})

(defn dot-fill
  "dot fill"
  [side content x y]
  (if (cell-content content x y)
    (get-in dot-fills [side :present])
    (get-in dot-fills [side :absent])))

(defn available-rights
  "return a seq of empty cells in the right grid"
  []
  (keep-indexed #(if (nil? %2) %1) (:rights @game)))

;;;
;; game state updates
;;;
(defn click-on
  "click on a dot"
  [[side [x y :as dot]]]
  (when dot
    (when (= side :left)
      (let [index (xy->index x y)
            number (nth (:lefts @game) index)]
        (when number
          (prn (str "click-on " side [x y]))
          (swap! game #(-> %
                           (assoc-in [:lefts index] nil)
                           (assoc-in [:rights (first (available-rights))] number))))))))

(defn commit-drag
  "commit a drag"
  [start-dot end-dot]
  (when end-dot
    (prn (str "commit drag " start-dot " -> " end-dot)))
  )

;;;;;;;;;

(r/defc grid
  [side-key content background-fill x0 nth-fill]
  [:g {:style {:cursor "pointer"}}
   [:rect {:x (gridy->svgy x0)
           :y (gridy->svgy -0.5)
           :width (gridy->svgy 9.5)
           :height (gridy->svgy 9.5)
           :fill background-fill
           :rx dot-radius
           }]
   (for [y (range 0 rows)
         x (range 0 cols)]
     (let [[x' y'] [(gridx->svgx side-key x) (gridy->svgy y)]
           n (nth content (+ x (* 10 y)))
           dx (if (< n 10) -3.5 (if (< n 100) -6.5 -10.5))
           dy 4.5]
       [:g {:key [x y]
            :transform (str "translate(" x' "," y' ")")}
        [:circle {:cx 0
                  :cy 0
                  :r dot-radius
                  :fill (nth-fill n)
                  :key "lc"
                  }]
        [:text {:x dx
                :y dy
                :fill "#ffffff"
                :font-size 12
                :key :lt}
         (cell-content content x y)]]))])

(r/defc left-grid [lefts]
  (grid
   :left lefts
   "rgba(255,100,100,0.3)"
   -0.5
   (constantly "rgba(255,100,130,1)")))

(r/defc right-grid [rights]
  (grid
   :right rights
   "rgba(100,100,255,0.2)"
   10
   (fn [n] (if n "rgb(73, 134, 255, 1)" "rgba(160,160,255,0.5)"))
   ))

;;;
;; gestures
;;;
(def drag-line (atom nil))

(defn drag-start
  "start dragging a dot"
  [event]
  (.preventDefault event)
  (let [g @game]
    (let [dot (mouse->dot (el "svg") event)]
      #_(prn (str "drag-start " dot))
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

(r/defc debug < r/reactive []
  (let [g (r/react game)]
    [:div
     [:p (str "lefts " (:lefts g))]
     [:p (str "rights " (:rights g))]]))

;;;
;; Put the app/game in here
;;;
(r/defc game-container < r/reactive []
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
