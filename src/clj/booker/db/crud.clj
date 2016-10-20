;;;; =========================================================
;;;; Функции для базовых CRUD запросов к сущностям
;;;; =========================================================
(ns booker.db.crud
  (:require [booker.utils :as u]
            [booker.db.sqlutils :as sql]
            [honeysql.helpers :as s]
            [clj-time.core :as ctime]))


(defn default-get-all-rows
  "Получить все записи по таблице (с учетом статуса)"
  [table-key]
  (sql/m-select (sql/q-base-select table-key)))


(defn get-by-id [table-key id]
  (sql/m-select-single (->
                         (s/select :*)
                         (s/from table-key)
                         (s/where [:= :id id]))))


(defn get-rows-where [table-key where-clauses]
  "Получить строки, удовлетворяющие условиям (проверка на статус вложенная)"
  (sql/m-select (-> (sql/q-base-select table-key)
                    (#(reduce (fn [prev-sql where-cl]
                                (-> prev-sql
                                    (u/when-or-skip (not (u/nil-or-empty? where-cl))
                                                    (fn[x](s/merge-where x where-cl)))))
                              %
                              where-clauses))
                    )))


(defn get-row-where [table-key where-clauses]
  "Получить строку с where"
  (let [rows (get-rows-where table-key [where-clauses])]
    (if (> (count rows) 1)
      (throw (Exception. "More than 1 row was found in get-row-where"))
      (first rows))))

;; (default-get-all-rows :catalogs_client)
;; (get-rows-where :catalogs_client [[:= :id 170]])


(defn default-convert-on-add
  "По умолчанию заполнение служ.полей при добавлении"
  [row]
  (assoc row :creation_datetime (ctime/now)
             :last_edit_datetime (ctime/now)
             :status 1))


(defn default-convert-on-edit
  "По умолчанию заполнение служ.полей при редактировании"
  [row]
  (assoc row :last_edit_datetime (ctime/now)))


(defn default-add-row
  "Добавить новую запись по дефолту (с заполнением служ.полей)"
  [table-key row]
  (sql/insert-with-return table-key row))


(defn default-edit-row
  "Редактировать запись по дефолту (с заполнением служ.полей)"
  [table-key id row]
  (sql/update-by-id table-key id row))


(defn default-archivate-row
  "Архивировать запись по дефолту"
  [table-key id]
  (sql/update-by-id table-key id {:status 2}))


(defn get-rows-w-params
  "Базовая ф-ция для получения строк по настройкам
  для запроса и переданным параметрам"
  [basemap params]
  (sql/get-w-params-from-basemap basemap params))


(defn get-count-w-params
  "Базовая ф-ция для получения кол-ва строк по настройкам
  для запроса и переданным параметрам"
  [basemap params]
  (sql/get-count-from-basemap basemap params))
