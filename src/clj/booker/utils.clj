;;;; =========================================================
;;;; Разнообразные утилиты
;;;; =========================================================
(ns booker.utils
  (:require [clojure.walk]
            [booker.settings :as sett]
            [booker.time-utils :as tu]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [clj-time.format :as tf]
            [clojure.string :refer [blank?]]))

(defn nil-or-empty? [x]
  (or (nil? x) (empty? x)))

;; ==================== for math ==================

(defn subtr-or-zero
  "Получить либо положительную разницу либо 0"
  [a b]
  (let [r (- a b)]
    (if (> r 0) r 0)))

;; ================================================

(defn get-uni
  "Универсальный способ получить из хм значение поля
  field - может быть ключевым полем или вектором"
  [hm field]
  (if (sequential? field)
    (get-in hm field)
    (get hm field)))



(defn conc-keyw
  "Слить кейворд с какими-то аргументами (строки или др. кейворды)
  и получить кейворд"
  [keyw & s]
  (keyword
    (apply str (cons (name keyw) (map #(if (keyword? %)
                                         (name %)
                                         (str %)) s)))))

(defn has-prefix?
  "Проверка, что строка имеет префикс вначале"
  [s prefix]
  (.startsWith s prefix))


(defn keys-has-prefix?
  "Проверка, что ключ-строка имеет префикс,
  который состоит из ключа и делимитера (н-р '__')"
  [s-key p-key p-delimiter]
  (has-prefix? (name s-key) (str (name p-key) p-delimiter)))


(defn keys-get-without-prefix
  [s-key p-key p-delimiter]
  (keyword (subs (name s-key) (count (str (name p-key) p-delimiter)))))


(defn hm->copy-w-keyws
  "Скопировать в новый хм указанные значения из старого хм"
  [hm keyws]
  (reduce (fn[m k]
            (assoc m k (get hm k)))
          {} keyws))


;------------------------------ конвертации -----------------------------------
(defn to-int
  "Конвертировать в int некоторое значение
  (может быть числом, строкой и т.д.)"
  [x]
  (when-not (nil? x)
    (if (integer? x)
      x
      (Integer/parseInt (str x)))))


(defn to-double
  "Конвертировать в double некоторое значение
  (может быть числом, строкой и т.д.)"
  [x]
  (when-not (nil? x)
    (if (or (float? x)
            (instance? Long x)
            (instance? Double x))
      x
      (Double/parseDouble (str x)))))



(defn to-boolean
  "Конвертировать в boolean некоторое значение
  (может быть строкой, булевым значением и т.д.)"
  [x]
  (when-not (nil? x)
    (if (instance? Boolean x)
      x
      (case (clojure.string/lower-case x)
        "true" true
        "false" false
        (throw (Exception. (str "Недопустимое значение для строки boolean: " x)))))))


(defn to-date
  "Конвертировать в joda.datetime"
  [x]
  (when-not (nil? x)
    (if (tu/joda-date? x)
      x
      (tu/parse x))))


(defn convert-in-map
  "Преобразовать сразу несколько полей в хм с нужной конвертацией
  field-required? - признак, что если поле отсутствует, то не добавляем
  "
  ([convert-fn hm fields] (convert-in-map true
                                          convert-fn
                                          hm
                                          fields))
  ([field-required? convert-fn hm fields]
   (reduce (fn [newm fkey]
             (let [fval (get hm fkey)]
               (if (and field-required? (nil? fval))
                 newm
                 (assoc newm fkey (convert-fn fval)))))
           hm fields)))

; конвертировать внутри хэшмапа поля в нужные
(def in-hm->ints (partial convert-in-map to-int))
(def in-hm->doubles (partial convert-in-map to-double))
(def in-hm->dates (partial convert-in-map to-date))
(def in-hm->booleans (partial convert-in-map to-boolean))
; добавить (или переписать) поля внутри хэшмапа на 0
(def in-hm->int-zeros (partial convert-in-map false (fn[_] 0)))
(def in-hm->float-zeros (partial convert-in-map false (fn[_] (float 0))))

; убрать поле из хэшмапа
(defn in-hm->remove [hm fields]
  (reduce (fn [newm fkey]
            (dissoc newm fkey))
          hm fields))


;;=======================================================================================

(defn add-to-vector-into-map
  [hmap fieldkey value]
  (update hmap fieldkey #(into [] (conj % value))))


(defn get-map-by-key
  "Найти хм с нужным ключом и значением в списке хм"
  [allmaps mkey mvalue]
  (first(filter (fn [m] (= (mkey m) mvalue)) allmaps)))


(defn get-map-by-id
  "Найти хм с нужным id в списке хм"
  [allmaps id]
  (get-map-by-key allmaps :id id))


(defn in-list?
  "Проверка, находится ли элемент в векторе или списке"
  [l value]
  (not (nil? (some #{value} l))))


(defn multi->
  "Для -> макроса, чтобы можно было прикреплять несколько функций

  prev: предыдущая часть макроса ->
  get-conv-fn: ф-ция конвертирования, к-ая получает параметр из вектора
               params-vector и возвращает функцию, которая будет вып-ся
               над prev поочередно
  params-vector: вектор с параметрами, чтобы получать ф-цию конвертирования
  "
  [prev get-conv-fn params-vector]
  (loop [rem-params params-vector
         result prev]
    (if (empty? rem-params)
      result
      (let [[param & remaining] rem-params]
        (recur remaining
               (-> result
                   ((get-conv-fn param))))))))

(defn all-vals-nil?
  "Проверка, что в хм все поля nil"
  [hm]
  (every? nil? (vals hm)))

(all-vals-nil? {:id nil :a nil :b 3})


(defn m-prefixes->inner-fields
  "В изначальном хм конвертирует поля с префиксами
  в отдельный внутрений хм внутри изначального

  init-hm: изначальный хм
  inner-fields: вектор с указанием пути для внутреннего хм
  prefix-key: префикс в виде ключа
  prefix-delim: делимитер для префикса
  "
  [init-hm inner-fields prefix-key prefix-delim]
  (let [base-hm (reduce-kv (fn [hm k v]
                             (if (not (keys-has-prefix? k prefix-key prefix-delim))
                               (assoc hm k v)
                               hm))
                           {}
                           init-hm)
        sub-hm (

                 reduce-kv (fn [hm k v]
                             (if (keys-has-prefix? k prefix-key prefix-delim)
                               (assoc hm (keys-get-without-prefix k prefix-key prefix-delim) v)
                               hm))
                           {}
                           init-hm)
        result-hm (assoc-in base-hm inner-fields
                            ; если все поля пустые, значит можно просто поставить nil
                            (if (all-vals-nil? sub-hm)
                              nil
                              sub-hm))
        ]
    result-hm))


(defn when-or-skip
  "Для -> макроса: если условие выполнилось,
  то применить ф-цию, иначе вернуть старое значение"
  [prev condition cond-fn]
  (do
;;     (println "when-or-skip")
;;     (println "prev: " prev)
;;     (println "cond: " condition)
;;     (println "cond-fn: " cond-fn)
    (if condition
      (cond-fn prev)
      prev)))


(defn vec-params->
  "Для -> макроса: применить несколько раз функцию conv-fn
  с параметрами, полученными из фукнции get-params-fn к списку vecs"
  [prev conv-fn vecs get-params-fn]
  (reduce (fn[p v-elem]
            (apply conv-fn (cons p (get-params-fn v-elem))))
          prev vecs))


;; ================== macros utils ====================
(defn func-on-last
  "Получить функцию с перечисленными аргументами
  и кастомными в конце"
  [func func-params & rest-args]
  `(~func ~@func-params ~@rest-args))

;(func-on-last println '(2 3) 4)

(defmacro not-nil-or-throw
  [exception-msg & body]
  `(let [v# (do ~@body)]
     (if (nil? v#)
       (throw (Exception. ~exception-msg))
       v#)))

;; (macroexpand '(not-nil-or-throw
;;                   "Error bla"
;;                   (get {:some 2} :a)))


;; (not-nil-or-throw
;;   "Error bla"
;;   (get {:some 2} :some))













