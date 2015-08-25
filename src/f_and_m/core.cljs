(ns ^:figwheel-always f_and_m.core
    (:require [rum :as r]
              [cljs.reader :as reader]
              [clojure.set :refer (intersection)]
              [cljsjs.react]
              [jayq.core :refer ($)]
              )
    (:require-macros [jayq.macros :refer [ready]])
  )

(enable-console-print!)

;; define game state once so it doesn't re-initialise on reload
(defonce game (atom {:title "Factors and Mulitples Game"}))

(defn el [id] (.getElementById js/document id))

;;;;;;;;;

(defn home-did-mount
  "jayq wrapped code"
  [state]
  (ready
   (.DataTable ($ :#example)))
  state)

;; data-table mixin - adds jQuery DataTable code on jQuery ready
(def data-table {:did-mount home-did-mount})

(r/defc home < r/reactive data-table []
  [:div [:h1 "Factors and Multiples in Rum!"]
   [:div.row
    [:div.col-md-6
     [:table#example.table.table-striped.table-bordered {:cell-spacing "0" :width "100%"}
      [:thead
       [:tr [:th "Name"]
        [:th "Age"]]]
      [:tbody
       [:tr [:td "Matthew"]
        [:td "26"]]
       [:tr [:td "Anna"]
        [:td "24"]]
       [:tr [:td "Michelle"]
        [:td "42"]]
       [:tr [:td "Frank"]
        [:td "46"]]]]]]
   ])

;;
;; Put the app/game in here
;;
(r/defc game-container < r/reactive []
  (home))


#_(defn home-component []
  (reagent/create-class {:reagent-render home
                         :component-did-mount home-did-mount
                         }))

;;;;;;;;;
;;
;; mount main component on html game element
;;
(r/mount (game-container) (el "game"))

(prn (el "game"))

;;
;; optionally do something on game reload
;;
(swap! game update-in [:__figwheel_counter] inc)
