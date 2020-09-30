(ns holiday-ping-ui.holidays.views
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [holiday-ping-ui.common.views :as views]
   [holiday-ping-ui.common.forms :as forms]
   [holiday-ping-ui.routes :as routes]
   [holiday-ping-ui.holidays.calendar :as calendar]))

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
  [channel-name current next selected]
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
          :on-click #(re-frame/dispatch [:holidays-save channel-name])}
         [:span "Save"]
         [:span.icon.is-small [:i.fa.fa-check]]]]]]]))

(defn edit-holiday-modal-form
  [{:keys [holiday-name date-string holiday?]}]
  (let [form           (reagent/atom {:name holiday-name})
        deselect-day   #(re-frame/dispatch [:calendar-deselect-day])
        save-holiday   #(re-frame/dispatch [:holidays-modal-save (:name @form)])
        remove-holiday #(re-frame/dispatch [:holidays-modal-remove])
        form-fields    [{:key      :name
                         :type     "text"
                         :required true}]]
    (fn [_]
      (let [valid-form? @(re-frame/subscribe [:valid-form? @form form-fields])]
        [:div.modal-card
         [:header.modal-card-head
          [:p.modal-card-title "Edit Holiday on " date-string]
          [:button.delete {:aria-label "close"
                           :on-click   deselect-day}]]
         [:section.modal-card-body
          [forms/detached-form-view form form-fields]]
         [:footer.modal-card-foot
          [:div.modal-button-group
           [:button.button {:on-click deselect-day} "Cancel"]
           (when holiday?
             [:button.button.is-danger {:on-click remove-holiday}
              [:span.icon.is-small [:i.fa.fa-times]]
              [:span "Remove"]])
           [:button.button.is-success
            {:on-click save-holiday
             :class    (when-not valid-form? "is-static")}
            [:span.icon.is-small [:i.fa.fa-check]]
            [:span "Save"]]]]]))))

(defn edit-holiday-modal
  []
  (let [date-info @(re-frame/subscribe [:calendar-selected-day])]
    [:div.modal
     (when date-info {:class "is-active"})
     [:div.modal-background
      {:on-click #(re-frame/dispatch [:calendar-deselect-day])}]
     (when date-info
       [edit-holiday-modal-form date-info])]))

(defn holidays-view
  [channel-name]
  (let [current-year  @(re-frame/subscribe [:current-year])
        next-year     (inc current-year)
        selected-year @(re-frame/subscribe [:calendar-selected-year])]
    [:div
     [edit-holiday-modal]
     [views/section
      [views/breadcrumbs [["Channels" "/"]
                          [channel-name (routes/url-for :channel-edit :channel channel-name)]
                          ["Holidays"]]]
      [:p.subtitle.has "Select the days of the year on which you want reminders."]
      [holiday-controls channel-name current-year next-year selected-year]
      [:div (when-not (= selected-year current-year) {:hidden true})
       [calendar/year-view current-year]]
      [:div (when-not (= selected-year next-year) {:hidden true})
       [calendar/year-view next-year]]
      [:br]
      [:br]
      [:p.has-text-centered "Missing a national holiday? "
       [:a {:href   "https://github.com/lambdaclass/holiday_pinger/issues/new"
            :target "blank"} "File an issue."]]]]))
