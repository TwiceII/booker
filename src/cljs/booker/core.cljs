(ns booker.core
  (:require [rum.core :as rum]))

(enable-console-print!)
(println "yay!")

(defonce app-state (atom {:text "Hello Chestnut!"}))


(rum/defc operations-panel-view
  []
  [:div
   [:div.ui.three.item.menu
    [:a.item.active "Расходы"]
    [:a.item "Переводы"]
    [:a.item "Доходы"]]
   [:div.ui.segment
    [:div.ui.form
     [:div.field
      [:label "Дата"]
      [:input {:type "text" :name "op-date"}]]
     [:div.field
      [:label "Счет"]
      [:input {:type "text" :name "op-account"}]]
     [:div.field
      [:label "Сумма"]
      [:input {:type "text" :name "op-summ"}]]
     [:div.field
      [:label "Комментарий"]
      [:textarea {:rows 2}]]
     [:button.ui.button "Записать"]]]])



(defn init
  []
  (rum/mount (operations-panel-view)
             (.getElementById js/document "operations-panel-col")))


(init)
