(ns ^:figwheel-always lights-out.core
    (:require[om.core :as om :include-macros true]
             [om.dom :as dom :include-macros true]
             [sablono.core :as html :refer-macros [html]]))

;; define your app data so that it doesn't get over-written on reload

(def SIZE 12)

(def app-state
  (atom {:squares
         (vec (repeat SIZE (vec (repeat SIZE {:blue? false}))))}))

(defn randomize
  []
  (swap! app-state assoc :squares
         (mapv (fn [_]
                 (mapv (fn [_] {:blue? (< (rand) 0.5)})
                       (range SIZE)))
               (range SIZE))))

(defn lights-switch
  [x y]
  (fn [state]
    (let [steps [[0 0] [1 0] [0 1] [-1 0] [0 -1]]
          steps (mapv (fn [[a b]] [(+ a x) (+ b y)]) steps)
          steps (filterv (fn [[a b]] (and (<= 0 a (dec SIZE))
                                         (<= 0 b (dec SIZE))))
                         steps)]
      (reduce (fn [state step]
                (update-in state [:squares
                                  (first step)
                                  (second step)
                                  :blue?]
                           not))
              state steps))))

(defn square
  [sq owner {:keys [x y]}]
  (reify
    om/IRender
    (render [_]
      (html [(if (:blue? sq)
               :div.square.blue
               :div.square.green)
             {:on-click (fn [e]
                          (swap! app-state (lights-switch x y)))}]))))

(defn board
  [data owner]
  (reify
    om/IRender
    (render [_]
      (html
       [:table
        [:tbody
         (for [x (range (count (:squares data)))]
           [:tr
            (for [y (range (count (first (:squares data))))]
              [:td
               (om/build square
                         (get-in data [:squares x y])
                         {:opts {:x x :y y}})])])]]))))

(om/root
 board
 app-state
 {:target (. js/document (getElementById "app"))})
