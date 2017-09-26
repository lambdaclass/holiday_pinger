(ns holiday-ping-ui.channels.forms
  (:require [clojure.string :as string]))

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
               :help-text "Defaults to HolidayPing"}
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
               :value "{\n  \"date\": \"2017-09-22\",\n  \"email\": \"john.doe@mail.com\",\n  \"message\" :\"This is a Holiday Ping test: John Doe will be out on holidays.\",\n  \"name\": \"Facundo Olano\"\n}"}]
   "email"   [{:key           :emails
               :type          "tags"
               :label         "Emails"
               :item-validate :valid-email?
               :required      true
               :help-text     "List of email addresses to send the reminder to."}
              ]})

(def reminders
  [{:key      :same-day
    :label    "Send a reminder on the same day."
    :type     "select"
    :options  [{:text "Yes" :value true}
               {:text "Don't send" :value false}]
    :required true}
   {:key     :days-before
    :label   "Send a reminder before the holiday."
    :type    "select"
    :options [{:text "Don't send" :value 0}
              {:text "The day before" :value 1}
              {:text "Three days before" :value 3}
              {:text "A week before" :value 7}]
    :required  true}])

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
  (merge (select-keys channel [:type :name])
         (:configuration channel)
         {:same-day    (:same_day channel)
          :days-before (or (:days_before channel) 0)}))

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
