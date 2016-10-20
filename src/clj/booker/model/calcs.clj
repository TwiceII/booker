;;;; =========================================================
;;;; Функции для вычислений (остатка на счете и т.д.)
;;;; =========================================================
(ns booker.model.calcs
  (:require [booker.db.sqlutils :as sql]
            [booker.db.crud :as crud]
            [honeysql.core :as honey-sql]
            [honeysql.helpers :as s]))


(defn where-date-range
  [date-field from-date to-date]
  (if (and from-date to-date)
    [:between date-field from-date to-date]
    (if from-date
      [:> date-field from-date]
      (if to-date
        [:< date-field to-date]
        nil))))

(where-date-range :date nil nil)


(defn all-operations-with-account
  "Получить все операции по счету
  (можно передавать даты с и по)"
  ([account-id] (all-operations-with-account account-id nil nil))
  ([account-id from-date to-date]
   (let [expense-ops (crud/get-rows-where :operations_expense
                                          [[:= :account_id account-id]
                                           (where-date-range :date from-date to-date)])
         income-ops (crud/get-rows-where :operations_income
                                          [[:= :account_id account-id]
                                           (where-date-range :date from-date to-date)])
         from-transfer-ops (crud/get-rows-where :operations_transfer
                                                [[:= :from_account_id account-id]
                                                 (where-date-range :date from-date to-date)])
         to-transfer-ops (crud/get-rows-where :operations_transfer
                                                [[:= :to_account_id account-id]
                                                 (where-date-range :date from-date to-date)])]
     [expense-ops
      income-ops
      from-transfer-ops
      to-transfer-ops])))



(all-operations-with-account 1)

(defn account-balance-by-date
  [account-id]
  (let [account (crud/get-by-id :accounts account-id)
        all-operations (all-operations-with-account account-id)]
    (-> ;; изначальный остаток на счете
        (:init_summ account)
        ;; отнимаем расходы
        (#(reduce (fn[s e-op] (- s (:summ e-op))) % (get all-operations 0)))
        ;; прибавляем доходы
        (#(reduce (fn[s i-op] (+ s (:summ i-op))) % (get all-operations 1)))
        ;; отнимаем переводы, происходившие С этого счета
        (#(reduce (fn[s from-tr-op] (- s (:summ from-tr-op))) % (get all-operations 2)))
        ;; прибавляем переводы, происходившие НА этот счет
        (#(reduce (fn[s to-tr-op] (+ s (:summ to-tr-op))) % (get all-operations 3))))))


;; (account-balance-by-date 2)























