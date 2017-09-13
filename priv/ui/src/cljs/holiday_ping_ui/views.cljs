(ns holiday-ping-ui.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as re-frame]
   [holiday-ping-ui.forms :as forms]
   [holiday-ping-ui.calendar :as calendar]
   [holiday-ping-ui.helpers.countries :as countries]))

(defn message-view []
  [:div
   (when-let [message @(re-frame/subscribe [:error-message])]
     [:article.message.is-danger
      [:div.message-body message]])
   (when-let [message @(re-frame/subscribe [:success-message])]
     [:article.message.is-success
      [:div.message-body message]])])

;; TODO review if still necessary/changed
(defn link-view
  [text event]
  [:a {:href "#" :on-click #(re-frame/dispatch event)} text])

(defn section
  "Wrap the given views in a regular section"
  [& views]
  [:section.section
   (apply vector :div.container views)])

(defn section-size
  "Take the bulma class for size as the first parameter, i.e. :is-half"
  [size & views]
  [:section.section
   [:div.container
    [:div.columns.is-centered
     (apply vector :div.column {:class (name size)} views)]]])

;;; AUTH VIEWS

(defn login-view []
  [:div
   [section-size :is-one-third
    [:div.card
     [:div.card-content
      [:div.has-text-centered
       [:a.button.is-medium.is-primary.is-fullwidth
        {:href "/oauth/github"}
        [:span.icon.is-medium [:i.fa.fa-github]]
        [:span
         " Login with GitHub"]]]
      [:hr]
      [message-view]
      [forms/form-view {:submit-text  "Login"
                        :submit-class "is-fullwidth"
                        :on-submit    [:auth-submit]
                        :fields       [{:key      :email
                                        :type     "email"
                                        :required true}
                                       {:key      :password
                                        :type     "password"
                                        :required true}]}]
      [:br]
      [:p.has-text-centered "Don't have an account? "
       [link-view "Click here to register." [:switch-view :register]]]]]]])

(defn register-view []
  (let [user-country @(re-frame/subscribe [:country])]
    [:div
     [section-size :is-half
      [:p.subtitle "Please fill your profile information."]
      [message-view]
      [forms/form-view {:submit-text "Register"
                        :on-submit   [:register-submit]
                        :fields      [{:key      :email
                                       :type     "email"
                                       :required true}
                                      {:key       :country
                                       :type      "select"
                                       :options   countries/list
                                       :value     user-country
                                       :help-text "We'll use this to load your default holidays."
                                       :required  true}
                                      {:key      :name
                                       :label    "Full name"
                                       :type     "text"
                                       :required true}
                                      {:key      :password
                                       :type     "password"
                                       :required true}
                                      {:key      :password-repeat
                                       :type     "password"
                                       :label    "Repeat password"
                                       :required true}]}]
      [:br]
      [:p.has-text-centered "Already registered? "
       [link-view "Click here to login." [:switch-view :login]]]]]))

(defn github-loading-view
  []
  [:div
   [section-size :is-one-third
    [:div.card
     [:div.card-content
      [:div.has-text-centered
       [:div.subtitle "Mining bitcoins..."]
       [:a.button.is-medium.is-primary.is-loading
        [:span
         " Login with GitHub"]]]]]]])

(defn github-register-view
  []
  (let [user-country @(re-frame/subscribe [:country])]
    [section
     [:p.subtitle "Please fill your profile information."]
     [message-view]
     [forms/form-view {:submit-text "Register"
                       :on-submit   [:github-register-submit]
                       :fields      [{:key       :country
                                      :type      "select"
                                      :options   countries/list
                                      :value     user-country
                                      :help-text "We'll use this to load you default holidays."
                                      :required  true}]}]]))
;;; CHANNEL VIEWS

(defn test-channel-modal
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

(defn channel-item-view
  [{:keys [name type] :as channel}]
  [:tr {:key name}
   [:td [:span.icon.is-small {:title "Slack"} [:i.fa.fa-slack]] "  " name]
   [:td.has-text-right
    [:div.field.is-grouped.is-grouped-right
     [:p.control
      [:button.button.is-danger.is-small
       {:on-click #(re-frame/dispatch [:channel-delete name])}
       [:span.icon.is-small [:i.fa.fa-times]]
       [:span "Delete"]]]
     [:p.control
      [:button.button.is-info.is-small
       {:on-click #(re-frame/dispatch [:channel-test-start channel])}
       [:span.icon.is-small [:i.fa.fa-cogs]]
       [:span "Test"]]]
     [:p.control
      [:button.button.is-info.is-small
       {:on-click #(re-frame/dispatch [:switch-view :channel-edit channel])}
       [:span.icon.is-small [:i.fa.fa-edit]]
       [:span "Edit"]]]]]])

(defn add-channel-button
  []
  [:p.has-text-centered
   [:button.button.is-success
    {:on-click #(re-frame/dispatch [:switch-view :channel-create])}
    [:span.icon.is-small [:i.fa.fa-plus]]
    [:span "New Channel"]]])

(defn channel-list-view
  []
  (let [channels @(re-frame/subscribe [:channels])]
    [:div
     [test-channel-modal]
     [section-size :is-two-thirds
      [message-view]
      [:p.subtitle.has-text-centered
       "Setup the channels you want to use to send your holiday reminders."]
      (when-not (empty? channels)
        [:table.table.is-fullwidth.is-striped
         [:tbody (map channel-item-view channels)]])
      [add-channel-button]]]))

(defn channel-add-view []
  [:div
   [section-size :is-half
    [:p.subtitle "Fill the channel configuration"]
    [message-view]
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
                                     :label "Bot emoji"}]}]]])

(defn channel-edit-view
  [channel]
  [:div
   [section-size :is-half
    [:p.subtitle "Fill the channel configuration"]
    [message-view]
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
                                     :label "Bot emoji"}]}]]])

;;; DASHBOARD views
;; (defn dashboard-view
;;   []
;;   (let [{username :name}     @(re-frame/subscribe [:user-info])
;;         {holiday-name :name
;;          date         :date} @(re-frame/subscribe [:next-holiday])
;;         count                @(re-frame/subscribe [:channel-count])
;;         count-text           (str "You have " (if (= 0 count) "no" count)
;;                                   " configured channels. ")
;;         holiday-text         (if holiday-name
;;                                (str "Your next holiday is " holiday-name " on " date ". ")
;;                                "You have no upcoming holidays ")]

;;     [section
;;      [:h1.title.is-3 (str "Hello, " username "!")]
;;      [:p.dashboard-message holiday-text
;;       [:a {:href "#" :on-click #(re-frame/dispatch [:switch-view :holidays])}
;;        " Manage holidays"]]
;;      [:br]
;;      [:p.dashboard-message count-text
;;       [:a {:href "#" :on-click #(re-frame/dispatch [:switch-view :channel-list])}
;;        " Manage channels"]]]))

;;; HOLIDAYS views
(defn holidays-year-switch
  [current next selected]
  [:div.field.has-addons
   [:p.control
    [:a.button
     (if (= selected current)
       {:class "is-static"}
       {:href "#" :on-click #(re-frame/dispatch [:calendar-select-year current])})
     current]]
   [:p.control
    [:a.button
     (if (= selected next)
       {:class "is-static"}
       {:href "#" :on-click #(re-frame/dispatch [:calendar-select-year next])})
     next]]])

(defn holiday-controls
  [current next selected]
  (let [edited? @(re-frame/subscribe [:calendar-edited?])
        empty?  @(re-frame/subscribe [:calendar-empty?])]
    [:nav.level
     [:div.level-left
      [:div.level-item
       [:div.level-item [holidays-year-switch current next selected]]]
      (when edited?
        [:div.level-item
         [:article.message.is-warning
          [:div.message-body "Your calendar has unsaved changes."]]])]
     [:div.level-right

      [:div.field.is-grouped
       [:p.control
        [:a.button.is-danger
         {:title    "Clear all holidays from the calendar"
          :href     "#"
          :class    (when empty? "is-static")
          :on-click #(re-frame/dispatch [:holidays-clear])}
         [:span "Clear"]
         [:span.icon.is-small [:i.fa.fa-times]]]]
       [:p.control
        [:a.button
         {:title    "Drop the changes made in the calendar"
          :href     "#"
          :class    (when-not edited? "is-static")
          :on-click #(re-frame/dispatch [:holidays-reset])}
         [:span "Reset"]
         [:span.icon.is-small [:i.fa.fa-undo]]]]
       [:p.control
        [:a.button.is-success
         {:title    "Save the changes in the calendar"
          :href     "#"
          :class    (when-not edited? "is-static")
          :on-click #(re-frame/dispatch [:holidays-save])}
         [:span "Save"]
         [:span.icon.is-small [:i.fa.fa-check]]]]]]]))

;; This is a bit too complex, try to refactor
(defn edit-holiday-modal
  []
  (let [date-info      @(re-frame/subscribe [:calendar-selected-day])
        date           (:date date-info)
        cancel-edit    #(re-frame/dispatch [:calendar-deselect-day])
        name-sub       (re-frame/subscribe [:calendar-selected-day-name])
        remove-holiday #(do (re-frame/dispatch [:holidays-remove date])
                            (re-frame/dispatch [:calendar-deselect-day]))
        save-holiday   #(do (re-frame/dispatch [:holidays-update date @name-sub])
                            (re-frame/dispatch [:calendar-deselect-day]))
        name-change    #(re-frame/dispatch [:calendar-selected-name-change (-> % .-target .-value)])]
    [:div.modal
     (when date {:class "is-active"})
     [:div.modal-background {:on-click cancel-edit}]
     [:div.modal-card
      [:header.modal-card-head
       (when date [:p.modal-card-title "Edit Holiday on " (:date-string date-info)])
       [:button.delete {:aria-label "close"
                        :on-click   cancel-edit}]]
      [:section.modal-card-body
       [:div.field
        [:div.control
         [:input.input {:type        "text"
                        :value       @name-sub
                        :on-change   name-change
                        :placeholder "Holiday name"}]]]]
      [:footer.modal-card-foot
       [:div.modal-button-group
        [:button.button {:on-click cancel-edit}
         "Cancel"]
        (when (:holiday? date-info)
          [:button.button.is-danger {:on-click remove-holiday}
           [:span.icon.is-small [:i.fa.fa-times]]
           [:span "Remove"]])
        [:button.button.is-success {:on-click save-holiday}
         [:span.icon.is-small [:i.fa.fa-check]]
         [:span "Save"]]]]]]))

(defn holidays-view
  []
  (let [current-year  @(re-frame/subscribe [:current-year])
        next-year     (inc current-year)
        selected-year @(re-frame/subscribe [:calendar-selected-year])]
    [:div
     [edit-holiday-modal]
     [section
      [:p.subtitle "Select the days of the year for which you want reminders."]
      [holiday-controls current-year next-year selected-year]
      [:div (when-not (= selected-year current-year) {:hidden true})
       [calendar/year-view current-year]]
      [:div (when-not (= selected-year next-year) {:hidden true})
       [calendar/year-view next-year]]]]))

;; REMINDER VIEWS
(defn reminder-config-view
  []
  (let [{:keys [same-day days-before]} @(re-frame/subscribe [:reminder-config])]
    [section
     [:p.subtitle "Configure the frequency of the holiday alerts."]
     [message-view]
     [forms/form-view {:submit-text "Save"
                       :on-submit   [:reminders-submit]
                       :fields      [{:key      :same-day
                                      :label    "Send a reminder on the same day."
                                      :type     "select"
                                      :value    same-day
                                      :options  [{:text "Yes" :value true}
                                                 {:text "Don't send" :value false}]
                                      :required true}
                                     {:key      :days-before
                                      :label    "Send a reminder before the holiday."
                                      :type     "select"
                                      :value    (or days-before 0)
                                      :options  [{:text "Don't send" :value 0}
                                                 {:text "The day before" :value 1}
                                                 {:text "Three days before" :value 3}
                                                 {:text "A week before" :value 7}]
                                      :required true}]}]]))

;; APP VIEWS
(defn user-info-view []
  (let [{name :name} @(re-frame/subscribe [:user-info])
        avatar       @(re-frame/subscribe [:avatar])]
    [:div.navbar-item.is-hoverable.has-dropdown
     [:a.navbar-link
      [:img {:src avatar}]]
     [:div.navbar-dropdown
      [:div.navbar-item.has-text-grey name]
      [:hr.navbar-divider]
      [:a.navbar-item "Settings"]
      [:a.navbar-item {:href "#" :on-click #(re-frame/dispatch [:logout])} "Logout"]]
     ]))

;; FIXME properly select the one that's active
(defn navbar-view
  []
  (let [authenticated? @(re-frame/subscribe [:access-token])]
    [:nav.navbar.has-shadow
     [:div.container

      [:div.navbar-brand
       [:div.navbar-item.is-size-3.app-title "HolidayPing"]
       [:div.navbar-burger.burger {:data-target "navMenubd"}]]

      (when authenticated?
        [:div#navMenubd.navbar-menu
         [:div.navbar-start
          [:a.navbar-item
           {:href   "https://notamonadtutorial.com"
            :target "_blank"}
           "Blog"]
          [:a.navbar-item
           {:href   "https://github.com/lambdaclass/holiday_ping"
            :target "_blank"}
           "GitHub"]]
         [:div.navbar-end
          [user-info-view]]])
      ][:hr.navbar-divider]
     ]))

(defn footer-view
  []
  [:footer.footer
   [:div.container
    [:div.content.has-text-centered
     [:p [:strong "HolidayPing"] " by "
      [:a {:href "https://github.com/lambdaclass/" :target "_blank"} "LambdaClass"] "."]
     [:p [:a.icon {:href "https://github.com/lambdaclass/holiday_ping"}
          [:i.fa.fa-github]]]]]])

(def views {:channel-list   [channel-list-view]
            :channel-edit   [channel-edit-view]
            :channel-create [channel-add-view]

            :login           [login-view]
            :register        [register-view]
            :github-loading  [github-loading-view]
            :github-register [github-register-view]
            :holidays        [holidays-view]
            })

(defn app
  "Build the ui based on the current-view in the app-db."
  []
  (let [current-view      @(re-frame/subscribe [:current-view])
        current-view-args @(re-frame/subscribe [:current-view-args])
        [view]            (get views current-view)]
    [:div
     [navbar-view]
     (apply vector view current-view-args)
     [footer-view]]))
