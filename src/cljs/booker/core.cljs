(ns booker.core
  (:require [rum.core :as rum]))

(enable-console-print!)
(println "yay!")

(defonce app-state (atom
                     {:last-operations [{:op-type :income
                                         :id 1
                                         :date "18.10.2016"
                                         :account-name "Кошелек"
                                         :category-name "Зарплата"
                                         :summ 500000
                                         :currency "KZT"}
                                        {:op-type :expense
                                         :id 1
                                         :date "19.10.2016"
                                         :account-name "Кошелек"
                                         :category-name "Такси"
                                         :summ 500
                                         :currency "KZT"}
                                        {:op-type :expense
                                         :id 2
                                         :date "19.10.2016"
                                         :account-name "Кошелек"
                                         :category-name "Еда"
                                         :summ 2000
                                         :currency "KZT"}
                                        ]
                      :account-balances [{:account-name "Кошелек"
                                          :account-summ 10000
                                          :currency "KZT"}
                                         {:account-name "Visa"
                                          :account-summ 100000
                                          :currency "KZT"}]
                      }))

(def app-cursor (partial rum/cursor-in app-state))

(defn total-account-balances
  "Получить всего баланса"
  [acc-balances]
  (reduce #(+ %1 (:account-summ %2))
          0 acc-balances))

;;; ====================================================
;;; Views
(rum/defc operations-panel-view
  []
  [:div
   [:div.ui.three.item.menu
    [:a.item.active "Расход"]
    [:a.item "Перевод"]
    [:a.item "Доход"]]
   [:div.ui.attached.segment
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


(rum/defc expense-operation-row-view
  [expense-op]
  [:div
   [:span.op-date (:date expense-op)] ":"
   [:span.op-account (:account-name expense-op)]
   [:i.long.arrow.right.orange.icon]
   [:span.op-category (:category-name expense-op)]
   [:span.op-summ (:summ expense-op) " " (:currency expense-op)]])

(rum/defc income-operation-row-view
  [income-op]
  [:div.item
   [:span.op-date (:date income-op)] ":"
   [:span.op-account (:account-name income-op)]
   [:i.long.arrow.left.green.icon]
   [:span.op-category (:category-name income-op)]
   [:span.op-summ (:summ income-op) " " (:currency income-op)]])

(rum/defc last-operations-view < rum/reactive
  [last-operations-cursor]
  (let [last-operations (rum/react last-operations-cursor)]
    [:div
     [:div.ui.top.attached.header "Последние операции"]
     [:div.ui.attached.segment
      [:div.ui.divided.list
       (map #(case (:op-type %)
               :expense (expense-operation-row-view %)
               :income (income-operation-row-view %))
            last-operations)]]
     ]))


(rum/defc account-balance-view
  [account-balance]
  [:div.item (:account-name account-balance) ": "
   (:account-summ account-balance) " "
   (:currency account-balance)])


(rum/defc balances-view < rum/reactive
  [account-balances-cursor]
  (let [account-balances (rum/react account-balances-cursor)
        total-balance (total-account-balances account-balances)]
    [:div.column
     [:div.ui.top.attached.header "Баланс: " total-balance " KZT"]
     [:div.ui.bottom.attached.segment
      [:div.ui.list
       (map account-balance-view  account-balances)]]]))

(defn init
  []
  (rum/mount (balances-view (app-cursor [:account-balances]))
             (.getElementById js/document "balances-row"))
  (rum/mount (operations-panel-view)
             (.getElementById js/document "operations-panel-col"))
  (rum/mount (last-operations-view (app-cursor [:last-operations]))
             (.getElementById js/document "last-operations-col"))
  )


(init)
