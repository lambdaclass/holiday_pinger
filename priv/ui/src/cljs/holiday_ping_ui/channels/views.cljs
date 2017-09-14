(ns holiday-ping-ui.channels.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as re-frame]
   [holiday-ping-ui.common.forms :as forms]
   [holiday-ping-ui.common.views :as views]))

(defn test-modal
  []
  (let [{name :name :as channel} @(re-frame/subscribe [:channel-to-test])
        cancel-test              #(re-frame/dispatch [:channel-test-cancel])
        confirm-test             #(re-frame/dispatch [:channel-test-confirm channel])]
    [:div.modal
     (when channel {:class "is-active"})
     [:div.modal-background {:on-click cancel-test}]
     [:div.modal-card
      [:header.modal-card-head
       [:p.modal-card-title "Test Channel"]
       [:button.delete {:aria-label "close"
                        :on-click   cancel-test}]]
      [:section.modal-card-body
       [:p "Do you want to test " [:b name] "? "]
       [:p "This will cause a test reminder to be delivered."]
       [:p]]
      [:footer.modal-card-foot
       [:div.modal-button-group
        [:button.button {:on-click cancel-test}
         "Cancel"]
        [:button.button.is-success {:on-click confirm-test}
         [:span.icon.is-small [:i.fa.fa-cogs]]
         [:span "Test"]]]]]]))

(defn item-view
  [{:keys [name type] :as channel}]
  [:tr {:key name}
   [:td
    [:div.title.is-5 name]
    [:div.subtitle.is-6 (str type " channel")]]
   [:td
    [:div.field.is-pulled-right.has-addons
     [:p.control
      [:button.button.is-danger.is-small.tooltip
       {:on-click     #(re-frame/dispatch [:channel-delete name])
        :data-tooltip "Delete"}
       [:span.icon.is-small [:i.fa.fa-times]]]]

     [:p.control
      [:button.button.is-info.is-small.tooltip
       {:on-click     #(re-frame/dispatch [:switch-view :channel-edit channel])
        :data-tooltip "Edit"}
       [:span.icon.is-small [:i.fa.fa-edit]]]]

     [:p.control
      [:button.button.is-small.tooltip
       {:on-click     #(re-frame/dispatch [:channel-test-start channel])
        :data-tooltip "Test channel"}
       [:span.icon.is-small [:i.fa.fa-cogs]]]]

     [:p.control
      [:button.button.is-small.tooltip
       {:on-click     #(re-frame/dispatch [:switch-view :holidays name])
        :data-tooltip "Select holidays"}
       [:span.icon.is-small [:i.fa.fa-calendar]]]]]]])

(defn add-button
  []
  [:p.has-text-centered
   [:button.button.is-success
    {:on-click #(re-frame/dispatch [:switch-view :channel-create])}
    [:span.icon.is-small [:i.fa.fa-plus]]
    [:span "New Channel"]]])

(defn list-view
  []
  (let [channels @(re-frame/subscribe [:channels])]
    [:div
     [test-modal]
     [views/section-size :is-two-thirds
      [views/message-view]
      [:p.subtitle.has-text-centered
       "Setup the channels to send your holiday reminders."]
      (when-not (empty? channels)
        [:table.table.is-fullwidth.is-outlined
         [:tbody (map item-view channels)]])
      [add-button]]]))

(defn add-view []
  [:div
   [views/section-size :is-half
    [:p.subtitle "Fill the channel configuration"]
    [views/message-view]
    [forms/form-view {:submit-text "Save"
                      :on-submit   [:channel-submit]
                      :on-cancel   [:switch-view :channel-list]
                      :fields      [{:key      :name
                                     :type     "text"
                                     :required true}
                                    {:key      :type
                                     :type     "select"
                                     :options  ["slack"]
                                     :value    "slack"
                                     :required true}
                                    {:key       :url
                                     :type      "text"
                                     :label     "Slack hook url"
                                     :help-text [:span "You can get the hook url "
                                                 [:a {:href   "https://my.slack.com/services/new/incoming-webhook/"
                                                      :target "blank"} "here."]]
                                     :required  true}
                                    {:key       :channels
                                     :type      "text"
                                     :label     "Targets"
                                     :required  true
                                     :help-text "Space separated, use \"#name\" for channels and \"@name\" for users."}
                                    {:key   :username
                                     :type  "text"
                                     :label "Bot username"}
                                    {:key   :emoji
                                     :type  "text"
                                     :label "Bot emoji"}
                                    {:key     :same-day
                                     :label   "Send a reminder on the same day."
                                     :type    "select"
                                     :value   true
                                     :options [{:text "Yes" :value true}
                                               {:text "Don't send" :value false}]}
                                    {:key     :days-before
                                     :label   "Send a reminder before the holiday."
                                     :type    "select"
                                     :value   0
                                     :options [{:text "Don't send" :value 0}
                                               {:text "The day before" :value 1}
                                               {:text "Three days before" :value 3}
                                               {:text "A week before" :value 7}]}]}]]])

(defn edit-view
  [channel]
  [:div
   [views/section-size :is-half
    [:p.subtitle "Fill the channel configuration"]
    [views/message-view]
    [forms/form-view {:submit-text "Save"
                      :on-submit   [:channel-submit]
                      :on-cancel   [:switch-view :channel-list]
                      :fields      [{:key      :name
                                     :type     "text"
                                     :value    (:name channel)
                                     :disabled true}
                                    {:key      :type
                                     :type     "select"
                                     :options  ["slack"]
                                     :value    "slack"
                                     :required true}
                                    {:key       :url
                                     :type      "text"
                                     :label     "Slack hook url"
                                     :value     (get-in channel [:configuration :url])
                                     :help-text [:span "You can get the hook url "
                                                 [:a {:href   "https://my.slack.com/services/new/incoming-webhook/"
                                                      :target "blank"} "here."]]
                                     :required  true}
                                    {:key       :channels
                                     :type      "text"
                                     :label     "Targets"
                                     :value     (string/join " " (get-in channel [:configuration :channels]))
                                     :required  true
                                     :help-text "Space separated, use \"#name\" for channels and \"@name\" for users."}
                                    {:key   :username
                                     :type  "text"
                                     :value (get-in channel [:configuration :username])
                                     :label "Bot username"}
                                    {:key   :emoji
                                     :type  "text"
                                     :value (get-in channel [:configuration :emoji])
                                     :label "Bot emoji"}
                                    {:key     :same-day
                                     :label   "Send a reminder on the same day."
                                     :type    "select"
                                     :value   (:same_day channel)
                                     :options [{:text "Yes" :value true}
                                               {:text "Don't send" :value false}]}
                                    {:key     :days-before
                                     :label   "Send a reminder before the holiday."
                                     :type    "select"
                                     :value   (or(:days_before channel) 0)
                                     :options [{:text "Don't send" :value 0}
                                               {:text "The day before" :value 1}
                                               {:text "Three days before" :value 3}
                                               {:text "A week before" :value 7}]}]}]]])
