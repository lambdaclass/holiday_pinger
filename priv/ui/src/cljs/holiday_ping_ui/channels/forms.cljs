(ns holiday-ping-ui.channels.forms)

(def channel-fields
  {"slack" [{:key       :url
             :type      "text"
             :label     "Slack hook url"
             :validate  :valid-slack-hook?
             :help-text [:span "You can get the hook url "
                         [:a {:href   "https://my.slack.com/services/new/incoming-webhook/"
                              :target "blank"} "here."]]
             :required  true}
            {:key       :targets
             :type      "text"
             :label     "Targets"
             :validate  :valid-slack-targets?
             :help-text "Where to send the message. Space separated, use \"#name\" for channels and \"@name\" for users. If left empty, the channel configured in the Slack hook will be used."}
            {:key       :username
             :type      "text"
             :label     "Bot username"
             :help-text "Defaults to HolidayPing"}
            {:key       :emoji
             :type      "text"
             :label     "Bot emoji"
             :help-text "Defaults to :calendar:"}]})

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
           {:key      :type
            :type     "text"
            :disabled true}]
          (get channel-fields (:type channel))
          reminders))
