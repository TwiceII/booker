;;;; =========================================================
;;;; Функции для работы с датами
;;;; =========================================================
(ns booker.time-utils
  (:require [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [clj-time.format :as tf]))

; форматтер для строк вида "2016-07-14T00:00:00Z" (для приходящих js дат)
(def iso-formatter (tf/formatters :date-time-no-ms))
; форматтер для дат вида "14.07.2016"
(def default-formatter (tf/formatter "dd.MM.yyyy"))
; форматтер для дат вида "2016-07-14"
(def ymd-formatter (tf/formatter "yyyy-MM-dd"))

(defn datetime-in-current
  "Получить датувремя с нужным часовым поясом (текущая датавремя)"
  [datetime]
  (t/to-time-zone datetime (t/time-zone-for-offset +6)))


(defn now-datetime
  "Получить текущую датувремя в нужном текущем поясе и т.д."
  []
  (datetime-in-current (t/now)))


(def time-formatter (tf/formatter "HH:mm:ss.SSS"))

(defn time->str
  "Получить строку времени в нашем часовом поясе"
  [dt]
  (tf/unparse (tf/with-zone time-formatter
                            (t/time-zone-for-offset +6))
              dt))


(defn datetime->str
  "Получить строку даты и времени в нашем часовом поясе в виде
  14.07.2016 13:04:45"
  [dt]
  (tf/unparse (tf/with-zone (tf/formatter "dd.MM.yyyy HH:mm:ss")
                            (t/time-zone-for-offset +6))
              dt))


(defn date->ddmmyyyy
  "Получить дату в строке ДД.ММ.ГГГГ"
  [dt]
  (tf/unparse default-formatter dt))


(defn date->ddmmyyyy-w-timezone
  "Получить дату в строке ДД.ММ.ГГГГ (для текущей зоны)"
  [dt]
  (tf/unparse (tf/with-zone default-formatter
                            (t/time-zone-for-offset +6))
              dt))


(defn current-date-str
  "Получить текущую дату в строке"
  []
  (tf/unparse (tf/formatter "yyyy-MM-dd") (now-datetime)))


(defn current-time-str
  "Получить текущее время в строке"
  []
  (time->str (t/now)))



(defn parse [date-str]
  "Парсит в нескольких форматах: js-iso, dd.MM.yyyy, yyyy-mm-dd"
  (try
    (tf/parse default-formatter date-str)
    (catch Exception e
      (try
        (tf/parse ymd-formatter date-str)
        (catch Exception e
          (tf/parse iso-formatter date-str))))))


(defn joda-date?
  "Проверка, что значение является org.joda.time.DateTime"
  [value]
  (instance? org.joda.time.DateTime value))


(defn joda-date->java-date
  "Конвертировать joda-date в java.util.Date"
  [joda-date]
  (tc/to-date (tc/to-local-date-time joda-date)))


(defn in-hm-joda-dates->java-dates
  "Преобразовать все записи в хм из joda date в java.utils.date"
  [obj]
  (let [convert-fn (fn [[k v]]
                     (if (joda-date? v)
                       [k (joda-date->java-date v)]
                       [k v]))]
    (clojure.walk/postwalk (fn [x]
                             (if (map? x)
                               (into {} (map convert-fn x))
                               x))
                           obj)))


(defn date-considering-weekends
  "Получить дату с учетом выходных
  (если выпадает на выходные - переносится на ПН след.недели"
  [d]
  (case (t/day-of-week d)
    6 (t/plus d (t/days 2))
    7 (t/plus d (t/days 1))
    d))


(defn dss-consider-weekends-and-date-to-check
  "Получает [{дата, сумма}] с учетом выходных
	и с учетом текущей даты"
  [init-dss date-to-check]
  (map #(if (not (t/before? (:date %) date-to-check))
          (assoc % :date (date-considering-weekends (:date %)))
          %) init-dss))


(defn base-sort-by-date-key
  "Сортировать список по полю :date "
  [desc? dates]
  (let [dates-in-vector (into [] dates)]
    (sort-by :date (if desc? t/after? t/before?) dates-in-vector)))


; от старых к новым
(def sort-by-date-key (partial base-sort-by-date-key false))
; от новых к старым
(def sort-by-date-key-desc (partial base-sort-by-date-key true))

;23.07.2016, 01:00

(defn parse-datetime-sync
  [dt-str]
  (datetime-in-current (tf/parse (tf/formatter "dd.MM.yyyy, HH:mm") dt-str)))

(defn parse-date-sync
  [dt-str]
  (datetime-in-current (tf/parse (tf/formatter "dd.MM.yyyy") dt-str)))

;; (parse-datetime-sync "23.07.2016, 01:00")
;; (parse-date-sync "23.07.2016")



















