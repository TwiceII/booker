;;;; =========================================================
;;;; Функции для работы с БД
;;;; =========================================================
(ns booker.db.sqlutils
  (:import [java.sql PreparedStatement])
  (:require [booker.utils :as u]
            [booker.time-utils :as tu]
            [booker.settings :as sett]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clojure.string :refer [blank?]]
            [clojure.java.jdbc :as jdbc]
            [honeysql.core :as honey-sql]
            [honeysql.helpers :as s]))

(let [db-host "localhost"
      db-port 5432
      db-name "booker"]
  (def db {:classname "org.postgresql.Driver" ; must be in classpath
           :subprotocol "postgresql"
           :subname (str "//" db-host ":" db-port "/" db-name)
           :user "postgres"
           :password "postgresadmin123"}))

;; (extend-type org.joda.time.DateTime
;;   json/JSONWriter
;;   (-write [date out]
;;     (json/-write (c/to-string date) out)))

;; ================================================================
;; для работы со временем
;; взято с http://rundis.github.io/blog/2015/clojure_dates.html
(extend-protocol jdbc/IResultSetReadColumn
  java.sql.Date
  ;(result-set-read-column [v _ _] (c/from-sql-date v))

;;   ; поменял на обычную java Date
;;   (result-set-read-column [v _ _] (.toDate (c/from-sql-date v)))

;;   java.sql.Timestamp
;;   (result-set-read-column [v _ _] (.toDate (c/from-sql-time v))))


  (result-set-read-column [v _ _] (tu/datetime-in-current (c/from-sql-date v)))

  java.sql.Timestamp
  (result-set-read-column [v _ _] (tu/datetime-in-current (c/from-sql-time v))))


(extend-type org.joda.time.DateTime
  jdbc/ISQLParameter
  (set-parameter [v ^PreparedStatement stmt idx]
    (.setTimestamp stmt idx (c/to-sql-time v))))
;; ================================================================

(defn r-select
  "Сырой sql-запрос в виде строки"
  [sql-str]
  (when sett/dev-mode
    (println "sql: " sql-str))
  (jdbc/query db sql-str))

;; (r-select "SELECT * FROM catalogs_client")

(defn m-select
  "Выполнить запрос sql, полученный в виде honey-sql мэпа"
  [sql-map]
  (r-select (honey-sql/format sql-map)))


(defn m-select-single
  "Выполнить запрос sql, полученный в виде honey-sql мэпа и вернуть единственное значение"
  [sql-map]
  (first (m-select sql-map)))


(defn insert-with-return
  "Вставить строку с возвратом id вставленной строки"
  ([table-name-key values] (insert-with-return table-name-key values db))
  ([table-name-key values conn]
   (jdbc/insert! conn table-name-key values)))


(defn update-by-field
  "Редактировать строку по какому-то полю
  row - мап с аргументами для SET"
  ([table-name-key fieldname fieldvalue row] (update-by-field table-name-key
                                                              fieldname fieldvalue
                                                              row db))
  ([table-name-key fieldname fieldvalue row conn]
   (jdbc/update! conn table-name-key row [(str fieldname " = ?") fieldvalue])))


(defn update-by-id
  "Редактировать строку по id
  row - мап с аргументами для SET"
  ([table-name-key id row] (update-by-id table-name-key id row db))
  ([table-name-key id row conn]
   (jdbc/update! conn table-name-key row ["id = ?" id])))




(defmacro in-transaction
  "Макрос для транзакций"
  [trans-name & body]
  `(jdbc/with-db-transaction [~trans-name db]
    ~@(map #(u/func-on-last (first %) (rest %) trans-name) body)))


(defmacro in-transaction-explicit
  "Макрос для транзакций
  (транзакция явно указывается во всех запросах)"
  [trans-name & body]
  `(jdbc/with-db-transaction [~trans-name db]
    ~@body))


(defn count-where
  "Получить кол-во из таблицы удовл.условию"
  [table-key where-hsql]
  (m-select-single (->  (s/select :%count.*)
                        (s/from table-key)
                        (s/where where-hsql))))


(defn has-where?
  "Есть ли в таблице строка, удовл.условию"
  [table-key where-hsql]
  (> (:count (count-where table-key where-hsql))
     0))


;; (macroexpand '(in-transaction tr
;;                 (insert-with-return :test1 {:status 345})
;;                 (when (= 1 1)
;;                   (update-by-id :test2 2 {:name "2some other name"}))))

;; =============================== для работы с запросами ==========================
(defn q-select-all
  "Базовый запрос получения всех строк в нужной таблице"
  [table-key]
  (-> (s/select :*)
      (s/from table-key)))


(defn q-base-select
  "Базовый запрос для таблицы с проверкой статуса"
  ([table-key] (q-base-select table-key nil))
  ([table-key prefix-key]
   (->
     (s/select (if prefix-key (u/conc-keyw prefix-key ".*") :*))
     (s/from (if prefix-key [table-key prefix-key] table-key))
     (s/where [:= (if prefix-key
                    (u/conc-keyw prefix-key ".status")
                    :status) true]))))

(defn where-substrs
  "Получить подзапрос по поиску подстроки в полях"
  [base-sql field-keys search-text]
  (if (and (not (u/nil-or-empty? field-keys))
           (not (blank? search-text)))
    (let [subqs (into [] (map #(-> [:like % (str "%" search-text "%")]) field-keys))]
      (s/merge-where base-sql
                     (into [] (cons :or subqs))))
    base-sql))


(defn where-substr
  "Получить подзапрос по поиску подстроки в поле"
  [base-sql field-key search-text]
  (if (and (not (blank? search-text)) field-key)
    (s/merge-where base-sql [:like field-key (str "%" search-text "%")])
    base-sql))


(defn limit-offset
  "Получить подзапрос для limit offset (для пейджинга)"
  [base-sql pagenumber pagesize]
  (if (and pagenumber pagesize)
    (let [offs (* pagesize (- pagenumber 1))
          lim pagesize]
      (-> base-sql (s/limit lim) (s/offset offs)))
    base-sql))


(defn get-order-fieldname [order-field]
  "Получить название поля для сортировки без '-'"
  (if (.startsWith order-field "-")
    (subs order-field 1)
    order-field))


(defn get-order-type-str [order-field]
  "Получить тип сортировки в виде '-' или пустое"
  (if (.startsWith order-field "-") "-" ""))


(defn order-by-field
  "Получить подзапрос для order by
  order-field - строка с названием поля, по которому сортировать
  (если вначале есть '-' - то по убыванию, иначе по возрастанию )"
  [base-sql order-field]
  (if order-field
    (let [field (get-order-fieldname order-field)
          order-type (if (.startsWith order-field "-") :desc :asc)]
      (s/order-by base-sql [(keyword field) order-type]))
    base-sql))


;; =============================== для работы с join ==========================
(def ^:const PREFIX-DELIMITER "__")

(defn column-names
  "Получить названия столбцов таблицы"
  [table-key]
  (m-select (->
              (s/select :column_name)
              (s/from :INFORMATION_SCHEMA.COLUMNS)
              (s/where [:= :table_name (name table-key)]))))

; мемоизированная версия
(def memo-column-names (memoize column-names))

(defn meta-get-prefixed-col-tuples
  "Получить таплы для нужной таблицы с префиксами для запросов
  (н-р 'c.name AS c__name...' и т.д.)"
  [table-key prefix]
  (map #(->
          [(keyword (str (name prefix) "." (:column_name %)))
           (str (name prefix) PREFIX-DELIMITER (:column_name %))])
       (memo-column-names table-key)))

;;        (m-select (->
;;                    (s/select :column_name)
;;                    (s/from :INFORMATION_SCHEMA.COLUMNS)
;;                    (s/where [:= :table_name (name table-key)])))))

(defn left-join-on-id
  "Добавить left join в запрос для таблицы"
  [base-sql table-key prefix-key on-field-key]
  (-> base-sql
      (#(apply s/merge-select % (meta-get-prefixed-col-tuples table-key
                                                              prefix-key)))
      (s/merge-left-join [table-key prefix-key]
                         [:= (keyword (str (name prefix-key) ".id")) on-field-key])))

(defn q-add-inner-fields
  "Добавить в запрос внутренние поля"
  [base-sql join-maps-vector]
  (-> base-sql
      (u/multi-> (fn [jm]
                   (fn [bsql]
                     (left-join-on-id bsql
                                      (:table-key jm)
                                      (:prefix-key jm)
                                      (:on-field-key jm))))
                 join-maps-vector)))


(defn inner-field-param
  "Получить хм параметров для join"
  [table-key prefix-key on-field-key inner-key-path]
  {:table-key table-key
   :prefix-key prefix-key
   :on-field-key on-field-key
   :inner-key-path inner-key-path})


(defn select-with-inner-fields
  "Получить строки с вложенными полями, указанными в параметрах join

  base-sql: основной запрос
  join-maps-vector: вектор с параметрами для join (см. inner-field-param)"
  [base-sql join-maps-vector]
  ; порядок важен в join maps vector - от общего к вложенному
  (let [all-rows (m-select
                   (-> base-sql
                       (q-add-inner-fields join-maps-vector)))]
    (map #(-> %
              (u/multi-> (fn [jm]
                           (fn [row]
                             (u/m-prefixes->inner-fields row
                                                         (:inner-key-path jm)
                                                         (:prefix-key jm)
                                                         PREFIX-DELIMITER)))
                         join-maps-vector))
         all-rows)))


;;; ===============================================================================
;;; Методы для стандартных запросов с передачей base-map-а (см. пример ниже)
;;; в основном для crud-select: типа вытащить все поля с таблицы с join-ами
;;; ===============================================================================

;; пример base-map-а:
;;  {:table :clientpayments
;;   :syn :cp
;;   :joins [[:catalogs_client :c :id :cp.client_id]
;;           [:primarydocs_clientcontract :cc :id
;;            :cp.client_contract_id [:name "contract_name"]]
;;           [:primarydocs_clientapplication :ca :id
;;            :cp.client_application_id [:name "app_name"]]]
;;   ;; необяз.поле
;;   :extra-selects []
;;   :search [:cp.name :ca.name :c.name :cc.name]
;;   :order-spec {"client" "client_name"
;;                "client_contract" "contract_name"
;;                "client_application" "app_name"}
;;   }

(defn- base-query-w-joins
  "Получить базовую часть запроса (без select) с join-ами,
  автоматическим where status = 1 и т.д."
  [main-table main-syn join-vecs]
  (-> (s/from [main-table main-syn])
      (s/where [:= (u/conc-keyw main-syn ".status") true])
      (u/vec-params-> s/merge-left-join
                    join-vecs
                    #(let [j-table (get % 0) j-syn (get % 1)
                           j-field (get % 2) j-on (get % 3)]
                       [[j-table j-syn]
                        [:= (u/conc-keyw j-syn (str "." (name j-field))) j-on]]))))


(defn base-map->base-query
  "Получить базовую часть запроса из base-map-а"
  [base-map]
  (base-query-w-joins (:table base-map)
                      (:syn base-map)
                      (:joins base-map)))


(defn selects-from-base-map
  "Получить select часть запроса
  (с доп. полями из join-таблиц, если передано внутри :joins base-map
  и доп.полями переданными внутри :extra-selects)"
  [query base-map]
  (-> query
      ;; все поля по умолчанию с основной таблицы
      (s/select (u/conc-keyw (:syn base-map) ".*"))
      ;; дополнительные поля с join-ов (если есть)
      (#(reduce (fn [q join-v]
                  (if (contains? join-v 4)
                    (let [j-syn (get join-v 1)
                          field-v (get join-v 4)]
                      (s/merge-select q [(u/conc-keyw j-syn "." (get field-v 0))
                                         (get field-v 1)]))
                    q))
                % (:joins base-map)))
      ;; дополнительные поля с extra-select-ов (если есть)
      (#(if (:extra-selects base-map)
          (reduce (fn [q es]
                      (s/merge-select q es))
                  % (:extra-selects base-map))
          %))
      ))


(defn base-map->order-by
  "Получить order-by часть запроса
  (с искл. случаями, если передано внутри :order-spec base-map)"
  [query base-map order-by]
  (let [order-fieldname (get-order-fieldname order-by)
        order-specs (:order-spec base-map)
        result-order-by (if (contains? order-specs order-fieldname)
                          (str (get-order-type-str order-by)
                               (get order-specs order-fieldname))
                          order-by)]
    (order-by-field query result-order-by)))


;; основные применяемые запросы для CRUD:
(defn query-count-from-basemap-and-params
  [base-map search-text]
  (-> (base-map->base-query base-map)
      (s/select :%count.*)
      (where-substrs (:search base-map) search-text)))


(defn get-count-from-basemap
  "Получить кол-во строк с поиском"
  ([base-map] (get-count-from-basemap base-map nil))
  ([base-map search-text]
   (m-select-single (query-count-from-basemap-and-params base-map
                                                         search-text))))

;;      (-> (base-map->base-query base-map)
;;          (s/select :%count.*)
;;          (where-substrs (:search base-map) search-text)
;;          ))))


(defn query-rows-from-basemap-and-params
  [base-map
   {:keys [pagesize pagenumber order-by search-text] :as params}]
  (-> (base-map->base-query base-map)
      (selects-from-base-map base-map)
      (u/when-or-skip search-text
                      #(where-substrs % (:search base-map) search-text))
      (u/when-or-skip (and pagenumber pagesize)
                      #(limit-offset % pagenumber pagesize))
      (u/when-or-skip order-by
                      #(base-map->order-by % base-map order-by))))


(defn get-w-params-from-basemap
  "Получить строки с join-ами с пейджингом, сортировкой, поиском)"
  [base-map
   {:keys [pagesize pagenumber order-by search-text] :as params}]
  (println "get-w-params-from-basemap")
  (println params)
  (m-select (query-rows-from-basemap-and-params base-map
                                                params)))

;;     (-> (base-map->base-query base-map)
;;                 (selects-from-base-map base-map)
;;                 (u/when-or-skip search-text
;;                                 #(where-substrs % (:search base-map) search-text))
;;                 (u/when-or-skip (and pagenumber pagesize)
;;                                 #(limit-offset % pagenumber pagesize))
;;                 (u/when-or-skip order-by
;;                                 #(base-map->order-by % base-map order-by)))))




;;; TEST
;; (get-w-params-from-basemap test-basemap {:order-by "client_application" :search-text "Asia"
;;                            :pagesize 20 :pagenumber 1 })
;; (get-count-from-basemap test-basemap "Asia")








































