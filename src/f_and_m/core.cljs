(ns ^:figwheel-always f_and_m.core
    (:require [rum.core :as rum]
              [cljsjs.react]
              [f-and-m.svg-event :as sve]
              ))

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
(def click-interval 1000)

;;;
;; utils
;;;
(defn sq [x] (* x x))

(defn sq-dist
  "square distance between points"
  [[x1 y1] [x2 y2]]
  (sq (+ (- x1 x2) (- y1 y2)))
  )

;;;
;; unit conversions
;;;
(defn gridx->svgx [side x]
  (condp = side
    :lefts (* (+ x 0.5) dot-radius 2)
    :rights (* (+ x cols 1) dot-radius 2)))
(defn gridy->svgy [y] (* (+ y 0.5) dot-radius 2))

(defn grid->svg [side [x y]]
  [(gridx->svgx side x) (gridy->svgy y)])

(defn svgx->gridx [x]
  (let [x' (/ x 2 dot-radius)]
    (if (<= x' 10)
      [:lefts (- x' 0.5)]
      [:rights (- x' cols 1)])))      ; inverse gridx->svgx

(defn svgy->gridy [y]
  (let [y' (/ y 2 dot-radius)]
    (- y' 0.5)))

(defn svg->grid
  "svg to side and grid coords"
  [[x y]]
  (let [[side x'] (svgx->gridx x)
        y' (svgy->gridy y)]
    [side [x' y']]))

(def viewport-width (gridx->svgx :rights (+ cols 0.5)))
(def viewport-height (gridy->svgy (- rows 0.5)))

;;;
;; define game state once so it doesn't re-initialise on reload
;;;e
(defonce game (atom {:title "Factors and Mulitples Game"
                     :lefts initial-lefts
                     :rights initial-rights
                     :drag-line nil}))

(defn el [id] (.getElementById js/document id))

;;;
;; game transforms
;;;
(defn grid->dot
  "game coords to dot coords. Return nil if not on a dot"
  [[side [x y]]]
  (let [[x' y'] [(.round js/Math x) (.round js/Math y)]]
    (if (<= (+ (sq (- x x')) (sq (- y y'))) 0.25)
      [side [x' y']]
      nil)))

(defn grid->nearest-dot
  "game coords to dot coords. Return nil if not on a dot"
  [[side [x y]]]
  (let [[x' y'] [(.round js/Math x) (.round js/Math y)]]
    [side [x' y']]))

(defn mouse->dot
  "find dot under mouse/touch point, Return nil if not on a dot"
  [svg-el event]
  (grid->dot (svg->grid (sve/mouse->svg svg-el event))))

(defn mouse->nearest-dot
  "find dot under mouse/touch point, Return nil if not on a dot"
  [svg-el event]
  (grid->nearest-dot (svg->grid (sve/mouse->svg svg-el event))))

;;;
;; painting
;;;
(defn xy->index [x y] (+ x (* cols y)))

(defn cell-content
  "dot number"
  [content x y]
  (nth content (xy->index x y))
  )

(def dot-fills {:lefts {:present "rgba(255,100,130,1)"
                       :absent "rgba(255,100,100,0)";"rgba(160,170,200,1)"
                       :active  "rgba(255,100,130,1)"}
                :rights {:present "rgba(59, 83, 254, 1)" ;"rgba(255,100,130,1)";"#3B53FE" ;
                        :chain-start "rgba(65, 150, 124, 1)"
                        :chained "rgba(80,180,80,1)"
                        :absent "rgba(160,160,255,0)"
                        :displaced "rgba(255,100,130,0.7)"}})

(defn f-or-m?
  "are m and n fctors or multiples of eah other"
  [m n]
  (and m n (= 0 (* (mod m n) (mod n m))))
  )

(defn chained?
  "determine whether a dot is in a factor/multiple chain"
  [content index]
  (let [number (nth content index)
        prior (when (> index 0) (nth content (- index 1)))
        next (when (< index (dec max-n)) (nth content (inc index)))]
    (or (and prior (f-or-m? prior number))
        (and next (f-or-m? number next)))))

(defn chain-start?
  "first item of a chain?"
  [content index]
  (let [number (nth content index)
        prior (when (> index 0) (nth content (- index 1)))
        next (when (< index (dec max-n)) (nth content (inc index)))]
    (and (or (not  prior) (not (f-or-m? prior number)))
        (and next (f-or-m? number next)))))

(defn chain-end?
  "last item of a chain?"
  [content index]
  (let [number (nth content index)
        prior (when (> index 0) (nth content (- index 1)))
        next (when (< index (dec max-n)) (nth content (inc index)))]
    (and (or (not  next) (not (f-or-m? next number)))
        (and prior (f-or-m? number prior)))))

(defn chain-length
  "find the length of a f_or_m chain from given index"
  [[longest len m] n]
  #_(prn (str "longest " longest " len " len " m " m " n " n))
  (if (f-or-m? m n) [(max longest (inc len)) (inc len) n] [longest 1 n]))

(defn longest-chain
  "calculate the length of the longest f-or-m chain"
  [content]
  (first (reduce chain-length [0 0 1] content))  )

(defn available-rights
  "return a seq of empty cells in the :rights grid"
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
      (if (= side :lefts)
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

(defn to-shift
  [content index]
  (take-while (complement nil?) (subvec content index)))


(defn insert-before-target-chain
  [source-index target-index]
  (let [rights (:rights @game)
        source-number (rights source-index)
        left-shift (to-shift rights (inc source-index))
        rights' (vec (concat (subvec rights 0 source-index)
                             left-shift
                             (subvec rights (+ source-index (inc (count left-shift))))))
        right-shift (to-shift rights' target-index)]
    (vec (concat (subvec rights' 0 target-index)
                            [source-number]
                            right-shift
                            (subvec rights' (+ target-index (count right-shift)))))))

(defn insert-left-before-target-chain
  [source-number target-index]
  (let [rights (:rights @game)
        right-shift (to-shift rights target-index)]
    (vec (concat (subvec rights 0 target-index)
                 [source-number]
                 right-shift
                 (subvec rights (+ target-index (count right-shift)))))))

(defn commit-drag
  "commit a drag"
  [[side [x y] :as start] [side' [x' y'] :as end]]
  (when (and start end)
    #_(prn (str "commit drag " side " " [x y] " -> " side' " " [x' y']))
    (cond
      (= side side' :rights)
      (let [content (side @game)
            source-index (xy->index x y)
            source-number (nth content source-index)
            target-index (xy->index x' y')
            ;target-index (if (> target-index' source-index) (inc target-index') target-index')
            target-number (nth content target-index)
            ]
        #_(prn source-index source-number target-index target-number)

        (when source-number
          (if target-number
            (swap! game #(assoc % :rights (insert-before-target-chain source-index target-index)))
            (swap! game #(-> %
                             (assoc-in [:rights target-index] source-number)
                             (assoc-in [:rights source-index] nil))))))

      (and (= side :lefts) (= side' :rights))
      (let [content (side @game)
            content' (side' @game)
            source-index (xy->index x y)
            source-number (nth content source-index)
            target-index (xy->index x' y')
            target-number (nth content' target-index)
            ]
       (when source-number
         (if target-number
           (do
             (swap! game #(assoc % :rights (insert-left-before-target-chain source-number target-index)))
             (swap! game #(assoc-in % [:lefts source-index] nil))
             )
            (swap! game #(-> %
                             (assoc-in [:rights target-index] source-number)
                             (assoc-in [:lefts source-index] nil)))))))))

(defn dxdy
  [number]
  [(if (< number 10) -3.5 (if (< number 100) -6.5 -10.5)) 4.5]
  )

(rum/defc render-line < rum/static [[x y] [x' y']]
  [:line {:x1 x :y1 y :x2 x' :y2 y'
          :stroke "black" :stroke-width 5 :stroke-linecap "round"
          :opacity 0.3}])

(rum/defc render-number < rum/static [number x' y' dx dy fill start-chain end-chain]
  [:g {:style {:cursor "pointer"}}
         [:circle {:cx x'
                   :cy y'
                   :r (- dot-radius 1)
                   :fill fill
                   :stroke (if (or start-chain end-chain) "black" fill)
                   :stroke-width 1
                   :stroke-dasharray (if start-chain "0,14,14" (if end-chain "14,28" ""))
                   :key :lc
                   }]
         [:text {:x (+ x' dx)
                 :y (+ y' dy)
                 :fill "#ffffff"
                 :font-size 12
                 :key :lt}
          number]])

(rum/defc dragged-cell < rum/cursored rum/cursored-watch [game]
  (let [drag-line @(rum/cursor game [:drag-line]) ]
    (when drag-line
      (let [[[side [x y]] [side' [x' y']] t] drag-line
            number (nth (side @game) (xy->index x y))
            [dx dy] (dxdy number)]
        #_(prn (str "dl " drag-line " side " side " xy " x "," y))
        [:g
         (rum/with-key
           (render-number number x' y' dx dy (get-in dot-fills [:rights :present]) false false)
           :dcn)
         (rum/with-key
           (render-line (grid->svg side [x y]) [x' y'])
           :dcl)]

        ))))

(rum/defc cell < rum/cursored rum/cursored-watch [game side-key x y]
  (let [[x' y'] [(gridx->svgx side-key x) (gridy->svgy y)]
        index (xy->index x y)
        number @(rum/cursor game [side-key index])
        ]
    (if number
      (let [content @(rum/cursor game [side-key])
            [dx dy] (dxdy number)
            fill (if (= side-key :lefts)
                   (get-in dot-fills [side-key :present])
                   (if (chained? content index)
                     (get-in dot-fills [side-key :chained])
                     #_(if (chain-start? content index)
                       (get-in dot-fills [side-key :chain-start])
                       (get-in dot-fills [side-key :chained]))
                     (get-in dot-fills [side-key :present])))
            right (= side-key :rights)
            ]
        (render-number number x' y' dx dy fill (and right (chain-start? content index)) (and right (chain-end? content index)))))))

(rum/defc grid < rum/static
  [side-key background-fill x0]
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
     (-> (cell game side-key x y)
         (rum/with-key [x y])))])

(rum/defc left-grid < rum/static [lefts]
  (grid
   :lefts
   "rgba(255,100,100,0.3)"
   -0.5))

(rum/defc right-grid < rum/static [rights]
  (grid
   :rights
   "rgba(100,170,100,0.2)"
   10))

;;;
;; gestures
;;;

;;
;; todo: fix: drag-line start uses grid coords, but drag-line end uses
;;

(defn drag-start
  "start dragging a dot"
  [event]
  (.preventDefault event)
  (let [g @game]
    (let [dot (mouse->dot (el "svg") event)]
      #_(prn (str "drag-start " dot))
      (swap! game assoc :drag-line [dot dot (.now js/Date)]))))

(defn drag-move
  "continue dragging a dot"
  [event]
  (.preventDefault event)
  (let [[x y :as end-xy] (sve/mouse->svg (el "svg") event)
        [side _] (svgx->gridx x)
        end-dot [side end-xy]
        [start-dot [_ last-xy] started-at :as dl] (:drag-line @game)]

    (when (and dl (> (sq-dist last-xy end-xy) 1))
      (swap! game assoc
             :drag-line [start-dot end-dot started-at]))))

(defn drag-stop
  "handle end of drag. Convert to a tap if not moved"
  [event]
  (.preventDefault event)
  (let [[side [x y] :as end-dot] (mouse->nearest-dot (el "svg") event)
        [start-dot _ started-at] (:drag-line @game)
        now (.now js/Date)]

    (when (and (>= x 0) (< x grid-size ) (>= y 0) (< y grid-size))
      (cond
        (and (= start-dot end-dot) (< (- now started-at) click-interval))
        (click-on end-dot)

        (= side :lefts)
        (click-on start-dot)

        :else
        (commit-drag start-dot end-dot)))
    (swap! game assoc :drag-line nil)))

(rum/defc svg-panel < rum/cursored rum/cursored-watch [game]
  [:svg {:view-box (str "0 0 " viewport-width " " viewport-height)
         :preserve-aspect-ratio "xMinYMin meet"
         :height "100%"
         :width "100%"
         :id "svg"
         :on-mouse-down drag-start
         :on-mouse-move drag-move
         :on-mouse-up drag-stop
         :on-touch-start drag-start
         :on-touch-move drag-move
         :on-touch-end drag-stop
         :style {:border "none !important"}}
   [:g
    (rum/with-key (left-grid @(rum/cursor game [:lefts])) "left")
    (rum/with-key (right-grid @(rum/cursor game [:rights])) "right")
    (when-let [[dl1 dl2] @(rum/cursor game [:drag-line])]
      (when (not= dl1 dl2)
        (rum/with-key (dragged-cell game) "dragged")))]])

(rum/defc debug < rum/reactive []
  (let [g (rum/react game)]
    [:div
     [:p {:key 1} (str "lefts " (:lefts g))]
     [:p {:key 2} (str "rights " (:rights g))]
     [:p {:key 3} (str "drag-line " (:drag-line g))]]))


(defn reset [event]
  (prn "foo")
  (.preventDefault event)
  (.stopPropagation event)
  (prn "fee")
  (swap! game #(assoc % :lefts initial-lefts
                      :rights initial-rights))
  )

(rum/defc reset-button < rum/reactive []
  [:button.btn.btn-primary {:on-mouse-up reset :on-touch-end reset} "Start again"]
  #_(if (some some? (:rights (rum/react game)))
    [:div {:style {:display "inline-block" :height "20px"}}]
    )
  )



;;;
;; Put the app/game in here
;;;
(rum/defc game-container < rum/cursored rum/cursored-watch [game]
  [:div {:class "panel panel-default" :style {:margin "2px"                                       :border "none !important"
}}
   [:div{:class "panel-heading"}
    [:h3.clearfix {:class "panel-title"}
     [:.pull-left {:style {:height "26px" :margin-top "6px"}} "Factors and Multiples "]
     [:.pull-right {:style {:height "26px" :margin-top "0px"}}
      "Longest Chain "
      (str (longest-chain @(rum/cursor game [:rights])) "  ")
      (reset-button)
      ]
     ]]
   [:div {:class "panel-body" :style {:height "800px"
                                      }}
    [:p "Click on a number to move it between the left and right squares.
Numbers in the right grid can be dragged to reorder them.
Aim to make the longest possible chain where each number is a factor or a multiple of its predecessor.
Each number may be used once only.
Chains are bracketed in green. Blue numbers are not part of a chain"]
    (svg-panel game)
    #_(debug)]])


(.initializeTouchEvents js/React true)

;;;
;; mount main component on html game element
;;;
(rum/mount (game-container game) (el "game"))

;;
;; optionally do something on game reload
;;
(defn on-js-reload []
  (swap! game update-in [:__figwheel_counter] inc))
