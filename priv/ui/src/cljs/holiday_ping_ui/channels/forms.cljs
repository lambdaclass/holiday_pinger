(ns holiday-ping-ui.channels.forms)

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
               :type      "text"
               :label     "Channels"
               :help-text "Space separated list of slack channels to post the reminder to. If left empty, the channel configured in the Slack hook will be used."}
              {:key       :users
               :type      "text"
               :label     "Users"
               :help-text "Space separated list of slack users to send the reminder to."}
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
               :value "{\n  \"date\": \"2017-09-22\",\n  \"email\": \"john.doe@mail.com\",\n  \"message\" :\"This is a Holiday Ping test: John Doe will be out on holidays.\",\n  \"name\": \"Facundo Olano\"\n}"}]})

(def reminders
  [{:key     :same-day
    :label   "Send a reminder on the same day."
    :type    "select"
    :options [{:text "Yes" :value true}
              {:text "Don't send" :value false}]}
   {:key     :days-before
    :label   "Send a reminder before the holiday."
    :type    "select"
    :options [{:text "Don't send" :value 0}
              {:text "The day before" :value 1}
              {:text "Three days before" :value 3}
              {:text "A week before" :value 7}]}])

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
            :disabled true}
           {:key       :type
            :type      "text"
            :read-only true}]
          (get channel-fields (:type channel))
          reminders))
