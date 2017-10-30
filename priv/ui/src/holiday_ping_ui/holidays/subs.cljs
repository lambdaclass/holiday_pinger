(ns holiday-ping-ui.holidays.subs
  (:require
   [re-frame.core :as re-frame]
   [cljs-time.core :as time]
   [holiday-ping-ui.common.subs :as subs]
   [holiday-ping-ui.common.time-format :as format]))

(subs/db-subscription :calendar-selected-year)
(subs/db-subscription :calendar-selected-day-name)

(defn- find-holiday
  [holidays date]
  (first (filter #(time/= date (:date %)) holidays)))

(defn- date-info
  [holidays date]
  (when date
    (let [as-holiday (find-holiday holidays date)]
      {:date          date
       :date-string   (format/date-to-string date)
       :today?        (time/= (time/today) date)
       :before-today? (time/before? date (time/today))
       :holiday?      (not (nil? as-holiday))
       :holiday-name  (:name as-holiday)})))

(re-frame/reg-sub
 :date-info
 (fn [{holidays :holidays-edited} [_ date]]
   (date-info holidays date)))

(re-frame/reg-sub
 :calendar-selected-day
 (fn [{:keys [calendar-selected-day holidays-edited]} _]
   (date-info holidays-edited calendar-selected-day)))

(re-frame/reg-sub
 :calendar-edited?
 (fn [{edited :holidays-edited
       saved  :holidays-saved} _]
   (not (time/= edited saved))))

(re-frame/reg-sub
 :calendar-empty?
 (fn [{holidays :holidays-edited} _]
   (let [upcoming? #(time/after? (:date %) (time/today))]
     (empty? (filter upcoming? holidays)))))

;; Return a range of month numbers for the given year.
;; If it's current year, return the range starting with the current month.
(re-frame/reg-sub
 :remaining-months-range
 (fn [_ [_ year]]
   (let [today (time/today)]
     (if (= (time/year today) year)
       (range (time/month today) 13)
       (range 1 13)))))

(re-frame/reg-sub
 :current-year
 (fn [_ _]
   (time/year (time/today))))
