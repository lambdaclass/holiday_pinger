(ns holiday-ping-ui.channels.forms
  (:require [clojure.string :as string]
            [cljs-time.core :as time]
            [cljs-time.periodic :as periodic]
            [holiday-ping-ui.common.time-format :as format]))

(def channel-fields
  {"slack"   [{:key       :url
               :type      "text"
               :label     "Slack hook url"
               :validate  :valid-slack-hook?
               :help-text [:span "You can get the hook url "
                           [:a {:href   "https://my.slack.com/services/new/incoming-webhook/"
                                :target "blank"} "here."]]
               :required  true}
              {:key       :channels
               :type      "tags"
               :label     "Channels"
               :help-text "List of slack channels to post the reminder to."}
              {:key       :users
               :type      "tags"
               :label     "Users"
               :help-text "List of slack users to send the reminder to."}
              {:key       :username
               :type      "text"
               :label     "Bot username"
               :help-text "Defaults to Holiday Pinger"}
              {:key       :emoji
               :type      "text"
               :label     "Bot emoji"
               :help-text "Defaults to :calendar:"}]
   "webhook" [{:key       :url
               :type      "text"
               :label     "Webhook url"
               :help-text "The url where we will post the holiday payload."
               :required  true}
              {:key       :secret
               :type      "password"
               :help-text "If secret is provided, we will use it to generate the HMAC digest of the request payload, which we will send base64 encoded in the X-Holiday-Signature header. "}
              {:key   :example-payload
               :type  "code"
               :label "Example payload"
               :value "{\n  \"date\": \"2017-09-22\",\n  \"email\": \"john.doe@mail.com\",\n  \"message\" :\"This is a Holiday Pinger test: John Doe will be out on holidays.\",\n  \"name\": \"Facundo Olano\"\n}"}]
   "email"   [{:key           :emails
               :type          "tags"
               :label         "Emails"
               :item-validate :valid-email?
               :required      true
               :help-text     "List of email addresses to send the reminder to."}
              ]})

(def hour-options
  (let [sequence (periodic/periodic-seq (time/today-at 00 00)
                                        (time/period :minutes 30))]
    (for [hour (take 48 sequence)]
      {:text  (format/time12-string hour)
       :value (format/time24-string hour)})))

(def default-timezone
  ;; cheating a bit here, I know
  (-> (js/Date.) .getTimezoneOffset format/utc-offset))

(def timezone-options
  [{:value "-12:00" :text "(GMT -12:00) Eniwetok, Kwajalein"}
   {:value "-11:00" :text "(GMT -11:00) Midway Island, Samoa"}
   {:value "-10:00" :text "(GMT -10:00) Hawaii"}
   {:value "-09:00" :text "(GMT -9:00) Alaska"}
   {:value "-08:00" :text "(GMT -8:00) Pacific Time (US & Canada)"}
   {:value "-07:00" :text "(GMT -7:00) Mountain Time (US & Canada)"}
   {:value "-06:00" :text "(GMT -6:00) Central Time (US & Canada), Mexico City"}
   {:value "-05:00" :text "(GMT -5:00) Eastern Time (US & Canada), Bogota, Lima"}
   {:value "-04:00" :text "(GMT -4:00) Atlantic Time (Canada), Caracas, La Paz"}
   {:value "-03:30" :text "(GMT -3:30) Newfoundland"}
   {:value "-03:00" :text "(GMT -3:00) Brazil, Buenos Aires, Georgetown"}
   {:value "-02:00" :text "(GMT -2:00) Mid-Atlantic"}
   {:value "-01:00" :text "(GMT -1:00 hour) Azores, Cape Verde Islands"}
   {:value "+00:00" :text "(GMT) Western Europe Time, London, Lisbon, Casablanca"}
   {:value "+01:00" :text "(GMT +1:00 hour) Brussels, Copenhagen, Madrid, Paris"}
   {:value "+02:00" :text "(GMT +2:00) Kaliningrad, South Africa"}
   {:value "+03:00" :text "(GMT +3:00) Baghdad, Riyadh, Moscow, St. Petersburg"}
   {:value "+03:30" :text "(GMT +3:30) Tehran"}
   {:value "+04:00" :text "(GMT +4:00) Abu Dhabi, Muscat, Baku, Tbilisi"}
   {:value "+04:30" :text "(GMT +4:30) Kabul"}
   {:value "+05:00" :text "(GMT +5:00) Ekaterinburg, Islamabad, Karachi, Tashkent"}
   {:value "+05:30" :text "(GMT +5:30) Bombay, Calcutta, Madras, New Delhi"}
   {:value "+05:45" :text "(GMT +5:45) Kathmandu"}
   {:value "+06:00" :text "(GMT +6:00) Almaty, Dhaka, Colombo"}
   {:value "+07:00" :text "(GMT +7:00) Bangkok, Hanoi, Jakarta"}
   {:value "+08:00" :text "(GMT +8:00) Beijing, Perth, Singapore, Hong Kong"}
   {:value "+09:00" :text "(GMT +9:00) Tokyo, Seoul, Osaka, Sapporo, Yakutsk"}
   {:value "+09:30" :text "(GMT +9:30) Adelaide, Darwin"}
   {:value "+10:00" :text "(GMT +10:00) Eastern Australia, Guam, Vladivostok"}
   {:value "+11:00" :text "(GMT +11:00) Magadan, Solomon Islands, New Caledonia"}
   {:value "+12:00" :text "(GMT +12:00) Auckland, Wellington, Fiji, Kamchatka"}])

(def reminders
  [{:key      :same-day
    :label    "Send a reminder on the same day."
    :type     "select"
    :options  [{:text "Yes" :value true}
               {:text "Don't send" :value false}]
    :required true}
   {:key      :days-before
    :label    "Send a reminder before the holiday."
    :type     "select"
    :options  [{:text "Don't send" :value 0}
               {:text "The day before" :value 1}
               {:text "Three days before" :value 3}
               {:text "A week before" :value 7}]
    :required true}
   {:key      :time
    :label    "Send the reminder at"
    :type     "select"
    :options  hour-options
    :required true}
   {:key      :timezone
    :label    "Using this time zone"
    :type     "select"
    :options  timezone-options
    :required true}])

(defn wizard-config-fields
  [type]
  (concat [{:key      :name
            :type     "text"
            :required true}]
          (get channel-fields type)))

(defn edit-fields
  [channel]
  (concat [{:key      :name
            :type     "text"
            :disabled true
            :required true}
           {:key       :type
            :type      "text"
            :read-only true
            :required  true}]
          (get channel-fields (:type channel))
          reminders))

(defn base-edit-defaults
  [channel]
  (let [days-before-array (:reminder_days_before channel)
        same-day?         (boolean (some #{0} days-before-array))
        days-before       (first (filter #(not= 0 %) days-before-array))]
    (merge (select-keys channel [:type :name])
           (:configuration channel)
           {:time        (:reminder_time channel)
            :timezone    (:reminder_timezone channel)
            :same-day    same-day?
            :days-before (or days-before 0)})))

(defmulti edit-defaults :type)
(defmethod edit-defaults :default
  [channel]
  (base-edit-defaults channel))

(defmethod edit-defaults "slack"
  [slack-channel]
  (let [targets  (get-in slack-channel [:configuration :channels])
        channels (filter #(string/starts-with? % "#") targets)
        users    (filter #(string/starts-with? % "@") targets)]
    (-> (base-edit-defaults slack-channel)
        (assoc :users users)
        (assoc :channels channels))))
