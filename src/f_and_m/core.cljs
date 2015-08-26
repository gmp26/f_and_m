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
(def viewport-width 430)
(def viewport-height 220)
(def scale-n 1)
(def rows 10)
(def max-n (* 10 rows))
(def initial-lefts (for [n (range 1 (inc max-n))] n))  ;; initial content of the left panel
(def initial-rights (for [n (range 1 (inc max-n))] nil))  ;; initial content of the left panel
(def unit 1)
(def gap 20)

;;;
;; define game state once so it doesn't re-initialise on reload
;;;
(defonce game (atom {:title "Factors and Mulitples Game"
                     :lefts initial-lefts
                     :rights initial-rights}))

(defn el [id] (.getElementById js/document id))

;;;
;; unit conversions
;;;
(defn units [x] (* x unit))
(defn stinu [x] (/ x unit)) ; inverse units

(defn gaps [x] (* (units x) gap))
(defn spag [x] (stinu (/ x gap))) ; inverse gaps

;;;
;; game transforms
;;;
(defn scale [factor]
  (fn [[x y]] [(* factor x) (* factor y)]))

(defn game->dot
  "game coords to integer game coords"
  [[x y]]
  [(.round js/Math x) (.round js/Math y)])

(defn svg->game
  "transform from svg viewport coords to game coords"
  [g]
  (comp
   (fn [[x y]] [(spag x) (spag y)])
   (scale (/ (:n g) scale-n ))))

(defn mouse->dot
  "find dot under mouse/touch point"
  [event]
  (game->dot ((svg->game @game) (sve/mouse->svg (el "grid") event))))

;;;;;;;;;

(r/defc left-grid [lefts]
  [:g
   (for [x (range 1 11)
         y (range 1 (inc rows))]
     (let [[x' y'] [(gaps x) (gaps y)]
           n (nth lefts (+ x -1 (* 10 (- y 1))))
           dx (if (< n 10) -3.5 (if (< n 100) -6.5 -10.5))
           dy 4.5]
       [:g {:key [x y]
            :transform (str "translate(" x' "," y' ")")}
        [:circle {:cx 0
                  :cy 0
                  :r (units 9)
                  :fill "rgba(230,100,130,1)"
                  :stroke "rgba(230,100,130,0.7)"
                  :stroke-width 8
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
     (let [[x' y'] [(gaps (+ x 10.5)) (gaps y)]
           n (nth rights (+ x -1 (* 10 (- y 1))))
           dx (if (< n 10) -3.5 (if (< n 100) -6.5 -10.5))
           dy 4.5]
       (prn (str "n = " n))
       [:g {:key [x y]
            :transform (str "translate(" x' "," y' ")")}
        [:circle {:cx 0
                  :cy 0
                  :r (units 9)
                  :fill (if n "rgba(230,100,130,1)" "rgba(190,170,255,1)")
                  :stroke (if n "rgba(230,100,130,0.7)" "rgba(190,170,230,0.6)")
                  :stroke-width 8
                  :key "lc"
                  }]
        [:text {:x dx
                :y dy
                :fill "#ffffff"
                :font-size 12
                :key :lt} n]]))])

(r/defc svg-panel < r/reactive []
  [:svg {:view-box (str "0 0 " viewport-width " " viewport-height)
         :height "100%"
         :width "100%"
         :id "grid"
         }
   [:g
    (left-grid (:lefts (r/react game)))
    (right-grid (:rights (r/react game)))]
   ])

;;;
;; Put the app/game in here
;;;
(r/defc game-container < r/reactive []
  [:div {:class "well"}
   (svg-panel)]
  )

;;;
;; mount main component on html game element
;;;
(r/mount (game-container) (el "game"))


;;
;; optionally do something on game reload
;;
(defn on-js-reload []
  (swap! game update-in [:__figwheel_counter] inc))


(defn show [n]
  (swap! game (fn [g] (assoc g :rights (concat
                                       (subvec (vec (:rights @game)) 0 3)
                                       [n]
                                       (subvec (vec (:rights @game)) 4 100)
                                       )))))
