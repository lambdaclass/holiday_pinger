(ns holiday-ping-ui.channels.subs
  (:require
   [clojure.string :as string]
   [re-frame.core :as re-frame]
   [holiday-ping-ui.common.subs :as subs]))

(subs/db-subscription :channels)
(subs/db-subscription :channel-to-test)
(subs/db-subscription :channel-to-edit)

(defn valid-slack-target?
  [value]
  (and
   (> (count value) 1)
   (or (string/starts-with? value "#")
       (string/starts-with? value "@"))))

(re-frame/reg-sub
 :valid-slack-targets?
 (fn [db [_ targets]]
   (let [targets (string/split targets #"\s+")]
     (if-not (every? valid-slack-target? targets)
       [false "Slack targets must start with @ or #"]
       [true]))))

(re-frame/reg-sub
 :valid-slack-hook?
 (fn [db [_ url]]
   (when url
     (let [url (string/trim url)]
       (if-not (or (nil? url) (string/starts-with? url "https://hooks.slack.com/"))
         [false "The url should be a valid slack hook url."]
         [true])))))

(re-frame/reg-sub
 :valid-slack-config?
 (fn [[_ form]]
   [(re-frame/subscribe [:valid-required? (:name form)])
    (re-frame/subscribe [:valid-required? (:url form)])
    (re-frame/subscribe [:valid-slack-targets? (:targets form)])
    (re-frame/subscribe [:valid-slack-hook? (:url form)])])
 (fn [validations _]
   (every? true? (map first validations))))
