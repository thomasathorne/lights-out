(ns ^:figwheel-always lights-out.core
    (:require[om.core :as om :include-macros true]
             [om.dom :as dom :include-macros true]
             [sablono.core :as html :refer-macros [html]]))

(def SIZE 4)

(def initial-squares (vec (repeat (* SIZE SIZE) {:on? false})))

(def app-state
  (atom {:squares initial-squares}))

(defn coord [n] [(quot n SIZE) (rem n SIZE)])

(defn uncoord [[x y]] (+ (* x SIZE) y))

(defn v-plus [[a b] [c d]] [(+ a c) (+ b d)])

(defn in-board [[x y]] (and (<= 0 x (dec SIZE)) (<= 0 y (dec SIZE))))

(def flip-shape [[0 0] [0 1] [0 -1] [-1 0] [1 0]])

(defn rel-flip-shape
  [n]
  (let [p (coord n)
        ps (mapv v-plus flip-shape (repeat p))
        ps (filterv in-board ps)]
    (mapv uncoord ps)))

(defn randomize
  []
  (mapv (fn [_] {:on? (< (rand) 0.5)})
        (range (* SIZE SIZE))))

(defn flip
  [squares n]
  (if (and n (<= 0 n (dec (* SIZE SIZE))))
    (update-in squares [n :on?] not)
    squares))

(defn flip+
  [squares n]
  (reduce flip squares (rel-flip-shape n)))

(defn square
  [sq owner {:keys [n]}]
  (reify
    om/IRender
    (render [_]
      (html [(if (:on? sq)
               :div.square.blue
               :div.square.green)
             {:on-click (fn [e]
                          (swap! app-state update-in [:squares] flip+ n))}]))))

(defn board
  [data owner]
  (reify
    om/IRender
    (render [_]
      (html
       [:table
        [:tbody
         (for [x (range SIZE)]
           [:tr
            (for [y (range SIZE)
                  :let [n (uncoord [x y])]]
              [:td
               (om/build square
                         (get-in data [:squares n])
                         {:opts {:n n}})])])]]))))

(om/root
 board
 app-state
 {:target (. js/document (getElementById "app"))})
