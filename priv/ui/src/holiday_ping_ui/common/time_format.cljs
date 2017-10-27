(ns holiday-ping-ui.common.time-format
  (:require
   [cljs-time.core :as time]
   [cljs-time.format :as format]))

(defn string-to-date
  [s]
  (let [formatter (format/formatters :date)]
    (format/parse-local-date formatter s)))

(defn date-to-string
  [d]
  (let [formatter (format/formatters :date)]
    (format/unparse formatter d)))

(defn month-string
  [year month]
  (let [date      (time/local-date year month 1)
        formatter (format/formatter "MMMM yyyy")]
    (format/unparse formatter date)))

(defn dd-mm-string
  [s]
  (let [date      (string-to-date s)
        formatter (format/formatter "dd/MM")]
    (format/unparse-local-date formatter date)))

(defn time12-string
  [d]
  (let [formatter (format/formatter "h:mma")]
    (format/unparse formatter d)))

(defn time24-string
  [d]
  (let [formatter (format/formatters :hour-minute)]
    (format/unparse formatter d)))

(defn utc-offset
  "Given an utc offset (minutes as returned by (new Date()).getTimezoneOffset())
  Return a UTC offset string like `-03:00`"
  [minutes]
  (let [abs       (max minutes (- minutes))
        date      (time/plus (time/today-at 00 00) (time/period :minutes abs))
        formatted (time24-string date)]
    (if (> minutes 0)
      (str "-" formatted)
      (str "+" formatted))))
