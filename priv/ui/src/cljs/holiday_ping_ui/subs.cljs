(ns holiday-ping-ui.subs
  (:require
   [clojure.string :as string]
   [re-frame.core :as re-frame]
   [goog.string :as gstring]
   [goog.crypt :as crypt]
   [cljs-time.core :as time]
   [goog.crypt.Md5]
   [holiday-ping-ui.helpers.time-format :as format]
   [holiday-ping-ui.helpers.token :as token]))

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
(db-subscription :calendar-selected-day-name)
(db-subscription :country)
(db-subscription :reminder-config)

(defn- next-holiday
  [holidays]
  (let [upcoming? #(time/after? (:date %) (time/today))]
    (first (filter upcoming? holidays))))

(re-frame/reg-sub
 :next-holiday
 (fn [{holidays :holidays-saved}]
   (when-let [next (next-holiday holidays)]
     (update next :date format/date-to-string))))

(re-frame/reg-sub
 :channel-count
 (fn [{channels :channels}]
   (when-not (nil? channels)
     (count channels))))

(re-frame/reg-sub
 :user-info
 (fn [_ _] (re-frame/subscribe [:access-token]))
 (fn [token _] (token/decode token)))

(defn- md5 [s]
  (let [bytes  (crypt/stringToUtf8ByteArray s)
        hasher (doto (goog.crypt.Md5.) (.update bytes))]
    (-> hasher .digest crypt/byteArrayToHex)))

(defn- gravatar
  [email]
  (->> email
       string/lower-case
       string/trim
       md5
       (gstring/format "https://www.gravatar.com/avatar/%s?d=identicon&s=64")))

(re-frame/reg-sub
 :avatar
 (fn [_ _] (re-frame/subscribe [:user-info]))
 (fn [{:keys [email avatar]} _]
   (if avatar
     avatar
     (gravatar email))))

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
 (fn [{edited :holidays-edited} _]
   (empty? (next-holiday edited))))

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
