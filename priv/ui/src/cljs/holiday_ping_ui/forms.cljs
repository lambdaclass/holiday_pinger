(ns holiday-ping-ui.forms
  (:require
   [clojure.string :as string]
   [reagent.core  :as reagent]
   [re-frame.core :as re-frame]))

(defn- field-handler
  "Return an event handler that updates the given key of the form atom."
  [form key]
  (fn [event]
    (let [value (-> event .-target .-value)]
      (swap! form assoc key value))))

(defn- field-name
  [{:keys [label key]}]
  (let [name_ (or label (name key))]
    (string/capitalize name_)))

(defmulti input-view
  "Multimethod that returns the hiccup component for an input field
  based on the type of the spec map."
  (fn [form field] (:type field)))

(defmethod input-view "select"
  [form field]
  [:div.select
   [:select {:on-change     (field-handler form (:key field))
             :default-value (get field :value "")}
    (for [option (:options field)
          :let   [value (get option :value option)
                  text  (get option :text option)]]
      [:option {:key value :value value} text])]])

(defmethod input-view :default
  [form {:keys [key type disabled] :as field}]
  (let [attrs {:type          type
               :name          (field-name field)
               :placeholder   (field-name field)
               :default-value (get @form key)
               :on-change     (field-handler form key)}]
    [:input.input (if disabled
                    (assoc attrs :disabled true)
                    attrs)]))

(defn- input-label
  [field]
  [:label.label [:b (field-name field) (when (:required field) "*") " "]])

(defn- get-defaults
  [fields]
  (reduce
   (fn [defaults field]
     (assoc defaults (:key field) (get field :value "")))
   {} fields))

(defn form-view
  "Generate the hiccup of a form based on a spec map."
  [{:keys [header-text submit-text submit-class on-submit on-cancel fields]}]
  (let [form (reagent/atom (get-defaults fields))]
    (fn []
      [:form
       [:div.content
        [:p header-text]]
       (for [{:keys [key help-text] :as field} fields]
         [:div.field {:key key}
          [input-label field]
          [:div.control
           [input-view form field]]
          (when help-text
            [:p.help help-text])])
       [:div.field.is-grouped.is-grouped-centered
        (when on-cancel
          [:div.control
           [:button.button.is-medium {:type     "button"
                                      :on-click #(re-frame/dispatch on-cancel)}
            "Cancel"]])
        [:button.button.is-primary.is-medium
         {:type     "submit"
          :class    submit-class
          :on-click (fn [event]
                      (re-frame/dispatch (conj on-submit @form))
                      (.preventDefault event))}
         submit-text]]])))
