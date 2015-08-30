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
      [:right (- x' cols 1)])))      ; inverse gridx->svgx

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
                       :absent "rgba(255,100,100,0)";"rgba(160,170,200,1)"
                       :active  "rgba(255,100,130,1)"}
                :right {:present "rgba(59, 83, 254, 1)" ;"rgba(255,100,130,1)";"#3B53FE" ;
                        :chain-start "rgba(70,170,40,1)"
                        :chained "rgba(80,180,80,0.7)"
                        :absent "rgba(160,160,255,0)"
                        :displaced "rgba(255,100,130,0.7)"}})

(defn f-or-m?
  "are m and n fctors or multiples of eah other"
  [m n]
  (and m n (= 0 (* (mod m n) (mod n m))))
  )

(defn chained?
  "determine whether a dot is in a factor/multiple chain"
  [content x y]
  (let [index (xy->index x y)
        number (nth content index)
        prior (when (> index 0) (nth content (- index 1)))
        next (when (< index max-n) (nth content (inc index)))]
    (or (and prior (f-or-m? prior number))
        (and next (f-or-m? number next)))))

(defn chain-start?
  "first item of a chain?"
  [content x y]
  (let [index (xy->index x y)
        number (nth content index)
        prior (when (> index 0) (nth content (- index 1)))
        next (when (< index max-n) (nth content (inc index)))]
    (and (or (not  prior) (not (f-or-m? prior number)))
        (and next (f-or-m? number next)))))

(defn chain-length
  "find the length of a f_or_m chain from given index"
  [[longest len m] n]
  #_(prn (str "longest " longest " len " len " m " m " n " n))
  (if (f-or-m? m n) [(max longest (inc len)) (inc len) n] [longest 0 n]))

(defn longest-chain
  "calculate the length of the longest f-or-m chain"
  [content]
  (first (reduce chain-length [0 0 1] content))  )

(defn dot-fill
  "dot fill"
  [side content x y]
  (if (cell-content content x y)
    (if (= side :left)
      (get-in dot-fills [side :present])
      (if (chained? content x y)
        (if (chain-start? content x y)
          (get-in dot-fills [side :chain-start])
          (get-in dot-fills [side :chained]))
        (get-in dot-fills [side :present])))
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
    (let [index (xy->index x y)]
      (if (= side :left)
        (let [number (nth (:lefts @game) index)]
          (when number
            #_(prn (str "click-on " side [x y]))
            (swap! game #(-> %
                             (assoc-in [:lefts index] nil)
                             (assoc-in [:rights (first (available-rights))] number)))))
        (let [number (nth (:rights @game) index)]
          (when number
            #_(prn (str "click-on " side [x y]))
            (swap! game #(-> %
                             (assoc-in [:lefts (- number 1)] number)
                             (assoc-in [:rights index] nil)))))))))

(defn must-shift
  [content target]
  (take-while (complement nil?) (subvec (:rights @game) target)
   ))

(defn commit-drag
  "commit a drag"
  [[side [x y] :as start] [side' [x' y'] :as end]]
  (when (and start end)
    (prn (str "commit drag " side " " [x y] " -> " side' " " [x' y']))
    (when (= side' :right)
      (let [content' (side' @game)
            source (xy->index x y)
            target (xy->index x y)
            shift (must-shift content' target)
            rights (concat
                    (subvec content' 0 target)
                    [(cell-content content' x y)]
                    shift
                    (subvec content' (+ target 1 (count shift)))) ]
        (swap! game #(assoc % side' rights)))
      ))
  )

;;;;;;;;;

(r/defc cell [side-key content x y xy-fill]
  (let [[x' y'] [(gridx->svgx side-key x) (gridy->svgy y)]
           number (cell-content content x y)
           dx (if (< number 10) -3.5 (if (< number 100) -6.5 -10.5))
           dy 4.5]
       [:g {:key [x y]
            :transform (str "translate(" x' "," y' ")")}
        [:circle {:cx 0
                  :cy 0
                  :r dot-radius
                  :fill (xy-fill x y)
                  :key "lc"
                  }]
        [:text {:x dx
                :y dy
                :fill "#ffffff"
                :font-size 12
                :key :lt}
         (cell-content content x y)]]))

(r/defc grid
  [side-key content background-fill x0 xy-fill]
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
     (-> (cell side-key content x y xy-fill)
         (r/with-key [x y])))])

(r/defc left-grid [lefts]
  (grid
   :left lefts
   "rgba(255,100,100,0.3)"
   -0.5
   (fn [x y] (dot-fill :left lefts x y))))

(r/defc right-grid [rights]
  (grid
   :right rights
   "rgba(100,170,100,0.2)"
   10
   (fn [x y] (dot-fill :right rights x y))))

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

(r/defc svg-panel []
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
   (let [g @game]
     [:g
      (r/with-key (left-grid (:lefts g)) "left")
      (r/with-key (right-grid (:rights g)) "right")])
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
     [:span {:style {:float "right"}}
      "Longest Chain "
      (str (longest-chain (:rights (r/react game))))]]]
   [:div {:class "panel-body"}
    [:p "Click on a number to move it between the left and right squares. Numbers in the right grid can be dragged to reorder them.
Aim to make the longest possible chain where each number is a factor or a multiple of its predecessor. Each number may be used once only. Valid chains are coloured green. The first number in a chain is dark green."]
    (svg-panel)
    #_(debug)]])


;;;
;; mount main component on html game element
;;;
(r/mount (game-container) (el "game"))


;;
;; optionally do something on game reload
;;
(defn on-js-reload []
  (swap! game update-in [:__figwheel_counter] inc))
