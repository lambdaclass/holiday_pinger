(ns holiday-ping-ui.holidays.views
  (:require
   [re-frame.core :as re-frame]
   [holiday-ping-ui.common.views :as views]
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
      [:p.subtitle.has "Select the days of the year for which you want reminders."]
      [holiday-controls channel-name current-year next-year selected-year]
      [:div (when-not (= selected-year current-year) {:hidden true})
       [calendar/year-view current-year]]
      [:div (when-not (= selected-year next-year) {:hidden true})
       [calendar/year-view next-year]]]]))
