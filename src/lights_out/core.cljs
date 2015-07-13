(ns ^:figwheel-always lights-out.core
    (:require[om.core :as om :include-macros true]
             [om.dom :as dom :include-macros true]
             [sablono.core :as html :refer-macros [html]]))

(def SIZE 4)

(def initial-squares (vec (repeat (* SIZE SIZE) {:on? false})))

(def app-state
  (atom {:squares initial-squares}))

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
  (let [to-flip [n
                 (if-not (= (dec SIZE) (rem n SIZE)) (inc n))
                 (if-not (= 0 (rem n SIZE)) (dec n))
                 (- n SIZE)
                 (+ n SIZE)]]
    (reduce flip squares to-flip)))

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
                  :let [n (+ (* y SIZE) x)]]
              [:td
               (om/build square
                         (get-in data [:squares n])
                         {:opts {:n n}})])])]]))))

(om/root
 board
 app-state
 {:target (. js/document (getElementById "app"))})
