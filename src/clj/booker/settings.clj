;;;; =========================================================
;;;; Настройки программы, константы
;;;; =========================================================
(ns booker.settings
  (:require [clj-time.core :as t]
            [clj-time.local :as tl]))

; включен ли режим разработки
; от этого зависит, какой код будет в html страницах
; и прочее
(def dev-mode true)


; бизнес-константы
;; (def business-consts
;;   {
;;    })


(defn main-date-to-check
  []
  "Получить дату (со временем 00:00:00),
  относительно которой проверяется просрочка и т.д"
  (let [now-d (t/now)]
    (tl/to-local-date-time
      (t/date-time (t/year now-d) (t/month now-d) (t/day now-d)))))
