(ns holiday-ping-ui.holidays.events
  (:require
   [re-frame.core :as re-frame]
   [ajax.core :as ajax]
   [cljs-time.core :as time]
   [holiday-ping-ui.common.events :as events]
   [holiday-ping-ui.holidays.format :as format]))

;;; HOLIDAY EVENTS
(defmethod events/load-view
  :holidays
  [{:keys [db]} [_ channel-name]]
  {:http-xhrio {:method          :get
                :uri             (str "/api/channels/" channel-name "/holidays")
                :timeout         8000
                :headers         {:authorization (str "Bearer " (:access-token db))}
                :response-format (ajax/json-response-format {:keywords? true})
                :on-success      [:holidays-load-success]
                :on-failure      [:error-message "Holidays loading failed."]}})

(re-frame/reg-event-db
 :holidays-load-success
 (fn [db [_ response]]
   (let [sorted   (sort-by :date response)
         holidays (map #(update % :date format/string-to-date) sorted)]
     (-> db
         (assoc :loading-view? false)
         (assoc :holidays-saved holidays)
         (assoc :holidays-edited holidays)))))

(re-frame/reg-event-fx
 :holidays-save
 (fn [{:keys [db]} [_ channel-name]]
   (let [edited       (:holidays-edited db)
         new-holidays (map #(update % :date format/date-to-string) edited)]
     {:http-xhrio {:method          :put
                   :uri             (str "/api/channels/" channel-name "/holidays")
                   :timeout         8000
                   :headers         {:authorization (str "Bearer " (:access-token db))}
                   :response-format (ajax/json-response-format {:keywords? true})
                   :format          (ajax/json-request-format)
                   :params          new-holidays
                   :on-success      [:holidays-load-success]
                   :on-failure      [:error-message "Holidays saving failed."]}})))

(re-frame/reg-event-db
 :holidays-reset
 (fn [db _]
   (assoc db :holidays-edited (:holidays-saved db))))

(re-frame/reg-event-db
 :holidays-clear
 (fn [db _]
   (let [past?         #(time/before? (:date %) (time/today))
         past-holidays (take-while past? (:holidays-saved db))]
     (assoc db :holidays-edited past-holidays))))

(re-frame/reg-event-db
 :calendar-select-year
 (fn [db [_ year]]
   (assoc db :calendar-selected-year year)))

(re-frame/reg-event-db
 :calendar-select-day
 (fn [db [_ day]]
   (let [holidays (:holidays-edited db)
         name     (:name (first (filter #(time/= day (:date %)) holidays)))]
     (-> db
         (assoc :calendar-selected-day day)
         (assoc :calendar-selected-day-name name)))))

(re-frame/reg-event-db
 :calendar-deselect-day
 (fn [db]
   (-> db
       (dissoc :calendar-selected-day)
       (dissoc :calendar-selected-day-name))))

(re-frame/reg-event-db
 :calendar-selected-name-change
 (fn [db [_ name]]
   (assoc db :calendar-selected-day-name name)))

(re-frame/reg-event-db
 :holidays-update
 (fn [db [_ date name]]
   (let [name (or name "")]
     (->> (:holidays-edited db)
          (remove #(time/= date (:date %)))
          (cons {:date date :name name})
          (sort-by :date time/before?)
          (assoc db :holidays-edited)))))

(re-frame/reg-event-db
 :holidays-remove
 (fn [db [_ date]]
   (let [edited  (:holidays-edited db)
         removed (remove #(time/= date (:date %)) edited)]
     (assoc db :holidays-edited removed))))
