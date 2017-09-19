(ns holiday-ping-ui.common.forms
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
  [form {:keys [key disabled options]}]
  [:div.select
   [:select {:on-change (field-handler form key)
             :value     (get @form key "")
             :disabled  disabled}
    (for [option options
          :let   [value (get option :value option)
                  text  (get option :text option)]]
      [:option {:key value :value value} text])]])

(defmethod input-view :default
  [form {:keys [key type disabled] :as field}]
  (let [attrs {:type        type
               :name        (field-name field)
               :placeholder (field-name field)
               :value       (get @form key)
               :on-change   (field-handler form key)}]
    [:input.input (if disabled
                    (assoc attrs :disabled true)
                    attrs)]))

(defn input-label
  [field]
  [:label.label [:b (field-name field) (when (:required field) "*") " "]])

(defn- get-defaults
  [fields]
  (reduce
   (fn [defaults field]
     (assoc defaults (:key field) (get field :value "")))
   {} fields))

(defn field-view
  [form {:keys [help-text] :as field}]
  [:div.field
   [input-label field]
   [:div.control
    [input-view form field]]
   (when help-text
     [:p.help help-text])])

(defn detached-form-view
  "Generate the hiccup of a form based on a spec map, using an external state
   atom."
  [form {:keys [fields]}]
  [:div
   (for [{:keys [key] :as field} fields]
     ^{:key key}[field-view form field])])

(defn form-view
  "Generate the hiccup of a form based on a spec map, with internally managed
  state."
  [{:keys [submit-text submit-class on-submit on-cancel fields] :as spec}]
  (let [form (reagent/atom (get-defaults fields))]
    (fn []
      [:form
       [detached-form-view form spec]
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
