(ns holiday-ping-ui.channels.subs
  (:require
   [clojure.string :as string]
   [re-frame.core :as re-frame]
   [cljs-time.core :as time]
   [clojure.contrib.humanize :as humanize]
   [holiday-ping-ui.common.time-format :as format]
   [holiday-ping-ui.common.subs :as subs]))

(subs/db-subscription :channels)
(subs/db-subscription :channel-to-test)
(subs/db-subscription :channel-to-edit)

(defn valid-slack-target?
  [value]
  (or (string/blank? value)
      (and
       (or (string/starts-with? value "#")
           (string/starts-with? value "@"))
       (> (count value) 1))))

(re-frame/reg-sub
 :valid-slack-targets?
 (fn [db [_ targets]]
   (let [targets (string/split targets #"\s+")]
     (if (or
          (empty? targets)
          (every? valid-slack-target? targets))
       [true]
       [false "Slack targets must start with @ or #"]))))

(re-frame/reg-sub
 :valid-slack-hook?
 (fn [db [_ url]]
   (when url
     (let [url (string/trim url)]
       (if-not (or (nil? url) (string/starts-with? url "https://hooks.slack.com/"))
         [false "The url should be a valid slack hook url."]
         [true])))))

(defn- next-holiday
  [holidays]
  (let [today     (format/date-to-string (time/today))
        upcoming? #(>= (:date %) today)]
    (first (filter upcoming? holidays))))

(re-frame/reg-sub
 :next-holiday
 (fn [db [_ {:keys [holidays]}]]
   (if-let [{:keys [date name]} (next-holiday holidays)]
     (str (format/dd-mm-string date) " - " name))))

(re-frame/reg-sub
 :last-reminder
 (fn [db [_ {:keys [reminders]}]]
   (if-let [{:keys [date]} (first reminders)]
     (let [date (format/string-to-date date)]
       (if (time/= date (time/today))
         "today"
         (humanize/datetime date))))))
