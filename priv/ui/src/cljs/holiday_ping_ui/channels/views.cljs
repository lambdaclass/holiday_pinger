(ns holiday-ping-ui.channels.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as re-frame]
   [reagent.core  :as reagent]
   [holiday-ping-ui.routes :as routes]
   [holiday-ping-ui.common.forms :as forms]
   [holiday-ping-ui.channels.forms :as channel-forms]
   [holiday-ping-ui.common.views :as views]
   [holiday-ping-ui.holidays.views :as holidays]
   [holiday-ping-ui.holidays.calendar :as calendar]))

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
       [:span.icon.is-small [:i.fa.fa-times]]]]]]])

(defn add-button
  []
  [:div.has-text-centered
   [:a.button.is-success {:href (routes/url-for :channel-create)}
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
                        :on-submit   [:channel-edit-submit]
                        :on-cancel   [:navigate :channel-list]
                        :defaults    (channel-forms/edit-defaults channel)
                        :fields      (channel-forms/edit-fields channel)}]]]))

;;; WIZARD VIEWS

(defn inc-step
  [state]
  #(swap! state update :step-n inc))

(defn dec-step
  [state]
  #(swap! state update :step-n dec))

(defn wizard-navigation
  "Show next/prev buttons to navigate between steps."
  [prev next]
  (let [show-prev?   (boolean prev)
        show-next?   (boolean next)
        static-class (when (:static next) "is-static")
        on-prev      (get prev :event prev)
        on-next      (get next :event next)]
    [:div
     [:br]
     [:nav.level
      [:div.level-left
       (when show-prev?
         [:div.level-item
          [:button.button
           {:on-click on-prev}
           [:span.icon.is-small
            [:i.fa.fa-chevron-left]]
           [:span "Prev"]]])]
      (when show-next?
        [:div.level-right
         [:div.level-item
          [:button.button.is-right
           {:class static-class :on-click on-next}
           [:span "Next"]
           [:span.icon.is-small
            [:i.fa.fa-chevron-right]]]]])]]))

(defn step-title
  [text]
  [:p.subtitle.is-5.has-text-centered text])

(defn select-type-event
  [state type]
  #(do (swap! state update :step-n inc)
       (swap! state assoc :type type)))

(defn type-card
  [state type title image]
  [:div.column.is-one-quarter
   [:div.card
    [:a {:href "#" :on-click (select-type-event state type)}
     [:header.card-header
      [:p.card-header-title title]]
     [:div.card-content
      [:figure.image.is-2by1
       [:img {:src image}]]]]]])

(defn type-select
  [wizard-state]
  [:div
   [step-title "Select the type of the channel you want to use."]
   [:br]
   [:div.columns.is-centered
    [type-card wizard-state "slack" "Slack" "/img/slack.png"]
    [type-card wizard-state "email" "Email" "/img/email.png"]
    [type-card wizard-state "webhook" "Webhooks" "/img/webhooks.png"]]])

(defn configuration-form
  [wizard-state type]
  (let [form-fields   (channel-forms/wizard-config-fields type)
        channel-state (reagent/cursor wizard-state [:channel-config])
        valid-form?   @(re-frame/subscribe [:valid-form? @channel-state form-fields])]
    [:div.columns.is-centered
     [:div.column.is-half
      [step-title "Fill the configuration for the integration."]
      [forms/detached-form-view channel-state form-fields]
      [wizard-navigation (dec-step wizard-state) {:static (not valid-form?)
                                                  :event  (inc-step wizard-state)}]]]))

(defn reminder-config-form
  [wizard-state]
  (let [reminder-state (reagent/cursor wizard-state [:reminder-config])]
    [:div.columns.is-centered
     [:div.column.is-half
      [step-title "When do you want the reminders sent?"]
      [forms/detached-form-view reminder-state channel-forms/reminders]
      [wizard-navigation (dec-step wizard-state) (inc-step wizard-state)]]]))

(defn holiday-source-form
  [wizard-state]
  (let [source-state (reagent/cursor wizard-state [:source-config])
        channels     @(re-frame/subscribe [:channels])
        source       (:source @source-state)]
    [:div.columns.is-centered
     [:div.column.is-half
      [step-title "What holidays do you want by default on your calendar?"]
      [:form
       [:div.field
        [:div.control
         [:label.radio
          [:input {:type      "radio"
                   :name      "from-country"
                   :checked   (= source :country)
                   :on-change #(swap! source-state assoc :source :country)}]
          " Use country defaults"]]]
       [:div.field
        [:div.control
         [forms/input-view source-state
          {:key      :country
           :type     "select"
           :disabled (not= source :country)
           :options  ["Argentina" "Brazil" "United States"]}]]]
       [:br]

       (when-not (empty? channels)
         [:div
          [:div.field
           [:div.control
            [:label.radio
             [:input {:type      "radio"
                      :name      "from-channel"
                      :checked   (= source :channel)
                      :on-change #(swap! source-state assoc :source :channel)}]
             " Copy from another channel"]]]
          [:div.field
           [:div.control
            [forms/input-view source-state
             {:key      :channel
              :type     "select"
              :disabled (not= source :channel)
              :options  (map :name channels)}]]]
          [:br]])

       [:div.field
        [:div.control
         [:label.radio
          [:input {:type      "radio"
                   :name      "empty"
                   :checked   (= source :empty)
                   :on-change #(swap! source-state assoc :source :empty)}]
          " Start with an empty calendar"]]]]

      [wizard-navigation
       (dec-step wizard-state)
       #(do (re-frame/dispatch [:load-base-holidays @source-state])
            (swap! wizard-state update :step-n inc))]]]))

(defn holiday-controls
  [wizard-state]
  (let [current-year  @(re-frame/subscribe [:current-year])
        next-year     (inc current-year)
        selected-year @(re-frame/subscribe [:calendar-selected-year])]
    [:nav.level
     [:div.level-left
      [:div.level-item
       [:button.button
        {:on-click (dec-step wizard-state)}
        [:span.icon.is-small
         [:i.fa.fa-chevron-left]]
        [:span "Prev"]]]]
     [:div.level-item.has-text-centered [holidays/holidays-year-switch current-year next-year selected-year]]
     [:div.level-right
      [:div.level-item
       [:button.button.is-right.is-success
        {:on-click #(re-frame/dispatch [:wizard-submit @wizard-state])}
        [:span "Save channel"]
        [:span.icon.is-small
         [:i.fa.fa-check]]]]]]))

(defn holiday-config
  [wizard-state]
  (let [current-year  @(re-frame/subscribe [:current-year])
        next-year     (inc current-year)
        selected-year @(re-frame/subscribe [:calendar-selected-year])]
    [:div
     [holidays/edit-holiday-modal]
     [step-title "Select the days of the year for which you want reminders."]
     [holiday-controls wizard-state]
     [:div (when-not (= selected-year current-year) {:hidden true})
      [calendar/year-view current-year]]
     [:div (when-not (= selected-year next-year) {:hidden true})
      [calendar/year-view next-year]]
     [:br]
     [holiday-controls wizard-state]]))

(defn wizard-steps
  "Show the wizard steps and navigate on click."
  [wizard-state step-n]
  [:div.columns.is-centered
   [:div.column.is-two-thirds
    [:div.steps.is-small
     (for [[i title] [[0 "Channel type"]
                      [1 "Channel config"]
                      [2 "Reminder config"]
                      [3 "Holiday sources"]
                      [4 "Calendar"]]]
       (cond
         (= i step-n)
         [:div.step-item.is-active
          {:key i}
          [:div.step-marker]
          [:div.step-content [:p.step-title title]]]

         (< i step-n)
         [:div.step-item.is-completed {:key i}
          [:a.step-marker
           {:href "#" :on-click #(swap! wizard-state assoc :step-n i)}
           [:span.icon [:i.fa.fa-check]]]
          [:div.step-content [:p.step-title title]]]

         (> i step-n)
         [:div.step-item {:key i}
          [:div.step-marker]
          [:div.step-content [:p.step-title title]]]))]]])

(def initial-wizard-state
  {:step-n          0
   :type            "slack"
   :channel-config  {}
   :reminder-config {:same-day    true
                     :days-before 0}
   :source-config   {:source  :country
                     :country "Argentina"}})

(def step-keys [:type-select
                :channel-config
                :reminder-config
                :holidays-source
                :holidays])

(defn create-view
  []
  (let [wizard-state (reagent/atom initial-wizard-state)]
    (fn []
      (let [step-n (:step-n @wizard-state)
            step   (get step-keys step-n)]
        [:div
         [views/section
          [views/breadcrumbs [["Channels" "/"] ["New"]]]
          [wizard-steps wizard-state step-n]
          (case step
            :type-select     [type-select wizard-state]
            :channel-config  [configuration-form wizard-state (:type @wizard-state)]
            :reminder-config [reminder-config-form wizard-state]
            :holidays-source [holiday-source-form wizard-state]
            :holidays        [holiday-config wizard-state])]]))))
