(ns holiday-ping-ui.channels.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as re-frame]
   [reagent.core  :as reagent]
   [holiday-ping-ui.routes :as routes]
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
      [:button.button.is-small.tooltip
       {:on-click     #(re-frame/dispatch [:channel-test-start channel])
        :data-tooltip "Test channel"}
       [:span.icon.is-small [:i.fa.fa-cogs]]]]
     [:p.control
      [:a.button.is-small.tooltip
       {:href         (routes/url-for :holidays :channel name)
        :data-tooltip "Select holidays"}
       [:span.icon.is-small [:i.fa.fa-calendar]]]]
     [:p.control
      [:a.button.is-info.is-small.tooltip
       {:href         (routes/url-for :channel-edit :channel name)
        :data-tooltip "Edit"}
       [:span.icon.is-small [:i.fa.fa-edit]]]]
     [:p.control
      [:button.button.is-danger.is-small.tooltip
       {:on-click     #(re-frame/dispatch [:channel-delete name])
        :data-tooltip "Delete"}
       [:span.icon.is-small [:i.fa.fa-times]]]]
     ]]])

(defn add-button
  []
  [:div.has-text-centered
   [:a.button.is-success {:href (routes/url-for :channel-type-select)}
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
      [add-button]
      [:br]
      (when-not (empty? channels)
        [:table.table.is-fullwidth.is-outlined
         [:tbody (map item-view channels)]])]]))

(defn type-select-view
  []
  [views/breadcrumbs [["Channels" "/"]
                      ["New"]]]
  [views/section
   [:p.subtitle "Select the type of the channel you want to use."]
   [:p "I'll make this prettier, promise"]
   [:p [:a {:href (routes/url-for :channel-create :type "slack")} "Slack"]]
   [:p [:a {:href (routes/url-for :channel-create :type "webhook")} "Webhook"]]
   [:p [:a {:href (routes/url-for :channel-create :type "email")} "Email"]]
   ])

(defn slack-config-form
  []
  [:p "slack config form"])

(defn webhook-config-form
  []
  [:p "webhook config form"])

(defn email-config-form
  []
  [:p "email config form"])

(defn reminder-config-form
  []
  [:p "reminder config"])

(defn holiday-source-form
  []
  [:p "holiday source"])

(defn holiday-config
  []
  [:p "calendar"])

(defn wizard-steps
  [wizard-state step-n]
  [:div.steps.is-small
   (for [[i title] [[0 "Channel config"]
                    [1 "Reminder config"]
                    [2 "Holiday sources"]
                    [3 "Calendar"]]]
     (cond
       (= i step-n)
       [:div.step-item.is-active
        [:div.step-marker]
        [:div.step-content [:p.step-title title]]]

       (< i step-n)
       [:div.step-item.is-completed

        [:a.step-marker
         {:href "#" :on-click #(swap! wizard-state assoc :step-n i)}
         [:span.icon [:i.fa.fa-check]]]
        [:div.step-content [:p.step-title title]]]

       (> i step-n)
       [:div.step-item
        [:div.step-marker]
        [:div.step-content [:p.step-title title]]]))])

(defn create-view
  [type]
  (let [step-keys    [:channel-config :reminder-config :holidays-source :holidays]
        wizard-state (reagent/atom {:step-n 0})]
    (fn []
      (let [step-n (:step-n @wizard-state)
            step   (get step-keys step-n)]
        [:div
         [views/section-size :is-half
          [views/breadcrumbs [["Channels" "/"]
                              ["New"]]]
          [wizard-steps wizard-state step-n]
          (case step
            :channel-config  (case type
                               "slack"   [slack-config-form]
                               "webhook" [webhook-config-form]
                               "email"   [email-config-form])
            :reminder-config [reminder-config-form]
            :holidays-source [holiday-source-form]
            :holidays        [holiday-config])
          [:nav.level
           [:div.level-left
            (when-not (= step :channel-config)
              [:div.level-item
               [:button.button
                {:on-click #(swap! wizard-state update :step-n dec)}
                [:span.icon.is-small
                 [:i.fa.fa-chevron-left]]
                [:span "Prev"]]])]
           (when-not (= step :holidays)
             [:div.level-right
              [:div.level-item
               [:button.button.is-right
                {:on-click #(swap! wizard-state update :step-n inc)}
                [:span "Next"]
                [:span.icon.is-small
                 [:i.fa.fa-chevron-right]]]]])]

          ]]))))

;; FIXME remove
(defn add-view []
  [:div
   [views/section-size :is-half
    [views/breadcrumbs [["Channels" "/"]
                        ["New"]]]
    [:div
     [:p.subtitle "Fill the channel configuration"]
     [views/message-view]
     [forms/form-view {:submit-text "Save"
                       :on-submit   [:channel-submit]
                       :on-cancel   [:navigate :channel-list]
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
                                                {:text "A week before" :value 7}]}]}]]]])

(defn edit-view
  [channel-name]
  (let [channel @(re-frame/subscribe [:channel-to-edit])]
    [:div
     [views/section-size :is-half
      [views/breadcrumbs [["Channels" "/"]
                          [channel-name (routes/url-for :channel-edit :channel channel-name)]]]
      [:p.subtitle "Fill the channel configuration"]
      [views/message-view]
      [forms/form-view {:submit-text "Save"
                        :on-submit   [:channel-submit]
                        :on-cancel   [:navigate :channel-list]
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
                                                 {:text "A week before" :value 7}]}]}]]]))
