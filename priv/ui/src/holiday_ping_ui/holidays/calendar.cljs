(ns holiday-ping-ui.holidays.calendar
  (:require
   [re-frame.core :as re-frame]
   [cljs-time.core :as time]
   [holiday-ping-ui.common.time-format :as format]))

(defn- first-of-calendar-month
  [year month]
  (let [first-of-month (time/local-date year month 1)
        left-offset    (- (time/day-of-week first-of-month) 1)]
    (time/minus first-of-month (time/days left-offset))))

(defn- last-of-calendar-month
  [year month]
  (let [last-of-month (time/last-day-of-the-month (time/local-date year month 1))
        right-offset  (- 7 (time/day-of-week last-of-month))]
    (time/plus last-of-month (time/days right-offset))))

(defn- month-seq
  "Get a sequence of dates to fill the weeks of the given month,
   including offset days from previous and next month."
  [year month]
  (let [first (first-of-calendar-month year month)
        last  (last-of-calendar-month year month)]
    (->> first
         (iterate #(time/plus % (time/days 1)))
         (take-while #(<= % last)))))

(defn date-button
  [month day holiday? today?]
  (let [button (cond
                 holiday? :button.date-item.is-active
                 today?   :button.date-item.is-today
                 :else    :button.date-item)]
    (when (= (time/month day) month)
      [button
       {:on-click #(re-frame/dispatch [:calendar-select-day day])}
       (time/day day)])))

(defn day-view
  [month day]
  (let [date-info @(re-frame/subscribe [:date-info day])
        today?    (:today? date-info)
        holiday?  (:holiday? date-info)
        date-div  (if (:before-today? date-info)
                    :div.calendar-date.is-disabled
                    :div.calendar-date)
        tooltip   (cond
                    holiday? (:holiday-name date-info)
                    today?   "Today"
                    :else    nil)]
    [date-div
     (when tooltip {:class "tooltip" :data-tooltip tooltip})
     [date-button month day holiday? today?]]))

(defn month-view
  [year month]
  [:div.calendar
   [:div.calendar-nav [:div (format/month-string year month)]]
   [:div.calendar-container
    [:div.calendar-header
     (for [day ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"]]
       [:div.calendar-date {:key day} day])]
    [:div.calendar-body
     (for [day (month-seq year month)]
       ^{:key day} [day-view month day])]]])

(defn year-view
  [year]
  [:div.container
   (let [month-range @(re-frame/subscribe [:remaining-months-range year])]
     (for [row (partition-all 3 month-range)]
       [:div.columns {:key (first row)}
        (for [month row]
          [:div.column.is-one-third-desktop.is-half-tablet {:key month}
           [month-view year month]])]))])
