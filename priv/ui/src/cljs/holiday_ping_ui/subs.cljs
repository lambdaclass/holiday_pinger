(ns holiday-ping-ui.subs
  (:require
   [clojure.string :as string]
   [re-frame.core :as re-frame]
   [goog.string :as gstring]
   [goog.crypt :as crypt]
   [goog.crypt.base64 :as base64]
   [cljs-time.core :as time]
   [goog.crypt.Md5]
   [holiday-ping-ui.time-format :as format]))

(defn db-subscription
  "Define a subscription handler that just gets a top level value from the db."
  ([db-kw] (db-subscription db-kw db-kw))
  ([sub-kw db-kw]
   (re-frame/reg-sub
    sub-kw
    (fn [db] (get db db-kw)))))

(db-subscription :access-token)
(db-subscription :channels)
(db-subscription :error-message)
(db-subscription :success-message)
(db-subscription :current-view)
(db-subscription :current-view-args)
(db-subscription :channel-to-test)
(db-subscription :calendar-selected-year)

(re-frame/reg-sub
 :next-holiday
 (fn [{holidays :holidays}]
   (let [upcoming? #(time/after? (:date %) (time/today))
         next      (first (filter upcoming? holidays))]
     (when next
       (update next :date format/date-to-string)))))

(re-frame/reg-sub
 :channel-count
 (fn [{channels :channels}]
   (when-not (nil? channels)
     (count channels))))

(defn- decode-token
  [token]
  (let [parse-json #(.parse js/JSON %)]
    (-> token
        (string/split #"\.")
        second
        base64/decodeString
        parse-json
        (js->clj :keywordize-keys true))))

(re-frame/reg-sub
 :user-info
 (fn [_ _] (re-frame/subscribe [:access-token]))
 (fn [token _] (decode-token token)))

(defn- md5 [s]
  (let [bytes  (crypt/stringToUtf8ByteArray s)
        hasher (doto (goog.crypt.Md5.) (.update bytes))]
    (-> hasher .digest crypt/byteArrayToHex)))

(re-frame/reg-sub
 :avatar
 (fn [_ _] (re-frame/subscribe [:user-info]))
 (fn [{email :email} _]
   (->> email
        string/lower-case
        string/trim
        md5
        (gstring/format "https://www.gravatar.com/avatar/%s?d=identicon&s=32"))))

(re-frame/reg-sub
 :date-info
 (fn [{holidays :holidays} [_ date]]
   (let [as-holiday (first (filter #(time/= date (:date %)) holidays))]
     {:today?        (time/= (time/today) date)
      :before-today? (time/before? date (time/today))
      :holiday?      (not (nil? as-holiday))
      :holiday-name  (:name as-holiday)})))

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
