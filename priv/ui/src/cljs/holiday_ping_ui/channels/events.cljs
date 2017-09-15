(ns holiday-ping-ui.channels.events
  (:require [clojure.string :as string]
            [re-frame.core :as re-frame]
            [ajax.core :as ajax]))

(re-frame/reg-event-fx
 :channel-load
 (fn [{:keys [db]} _]
   {:http-xhrio {:method          :get
                 :uri             "/api/channels"
                 :timeout         8000
                 :headers         {:authorization (str "Bearer " (:access-token db))}
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success      [:channel-load-success]
                 :on-failure      [:error-message "Channel loading failed."]}}))

(re-frame/reg-event-db
 :channel-load-success
 (fn [db [_ response]]
   (assoc db :channels response)))

(re-frame/reg-event-fx
 :channel-delete
 (fn [{db :db} [_ channel]]
   {:http-xhrio {:method          :delete
                 :uri             (str "/api/channels/" channel)
                 :timeout         8000
                 :headers         {:authorization (str "Bearer " (:access-token db))}
                 :format          (ajax/json-request-format)
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success      [:channel-delete-success channel]
                 :on-failure      [:error-message "Channel deleting failed."]}}))

(re-frame/reg-event-db
 :channel-delete-success
 (fn [db [_ channel]]
   (update db :channels
           (fn [channels]
             (remove #(= (:name %) channel) channels)))))

(defn valid-slack-target?
  [value]
  (or (string/starts-with? value "#")
      (string/starts-with? value "@")))

(re-frame/reg-event-fx
 :channel-submit
 (fn [{db :db} [_ {:keys [name type url channels username emoji days-before
                          same-day]}]]
   (let [channels    (string/split channels #"\s+")
         days-before (js/parseInt days-before)
         params      {:name          name
                      :type          type
                      :same_day      same-day
                      :days_before   (if (zero? days-before) nil days-before)
                      :configuration {:channels channels
                                      :url      url
                                      :username username
                                      :emoji    emoji}}]
     (cond
       (some string/blank? [name type url])
       {:dispatch [:error-message "Please fill required fields."]}

       (not (every? valid-slack-target? channels))
       {:dispatch [:error-message "Slack targets must start with @ or #"]}

       (not (string/starts-with? url "https://hooks.slack.com/"))
       {:dispatch [:error-message "The url should be a valid slack hook url."]}

       :else {:http-xhrio {:method          :put
                           :uri             (str "/api/channels/" name)
                           :headers         {:authorization (str "Bearer " (:access-token db))}
                           :timeout         8000
                           :format          (ajax/json-request-format)
                           :params          params
                           :response-format (ajax/text-response-format)
                           :on-success      [:channel-submit-success]
                           :on-failure      [:error-message "Channel submission failed"]}}))))

(re-frame/reg-event-fx
 :channel-submit-success
 (fn [_ _]
   {:dispatch-n [[:channel-load]
                 [:navigate :channel-list]]}))

(re-frame/reg-event-db
 :channel-test-start
 (fn [db [_ channel]]
   (assoc db :channel-to-test channel)))

(re-frame/reg-event-db
 :channel-test-cancel
 (fn [db _]
   (dissoc db :channel-to-test)))

(re-frame/reg-event-fx
 :channel-test-confirm
 (fn [{:keys [db]} [_ channel]]
   {:dispatch   [:channel-test-cancel]
    :http-xhrio {:method          :post
                 :uri             (str "/api/channels/" (:name channel) "/test")
                 :timeout         8000
                 :headers         {:authorization (str "Bearer " (:access-token db))}
                 :format          (ajax/json-request-format)
                 :params          {}
                 :response-format (ajax/text-response-format)
                 :on-success      [:success-message "Channel reminder sent."]
                 :on-failure      [:error-message "There was an error sending the reminder."]}}))
