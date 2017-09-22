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

(defn- validate
  [form {:keys [key validate required]}]
  (let [value (key @form)]
    (cond
      (nil? value)         [true] ;; dont validate before entering values
      (and value validate) @(re-frame/subscribe [validate value @form])
      required             @(re-frame/subscribe [:valid-required? value])
      :else                [true])))

(defmulti input-view
  "Multimethod that returns the hiccup component for an input field
  based on the type of the spec map."
  (fn [form field] (:type field)))

(defmethod input-view :default
  [form {:keys [key type disabled read-only value] :as field}]
  (let [[valid? message] (validate form field)
        attrs            {:type        type
                          :name        (field-name field)
                          :placeholder (field-name field)
                          :class       (when-not valid? "is-danger")
                          :value       (get @form key value)
                          :on-change   (field-handler form key)
                          :read-only   read-only
                          :disabled    disabled}]
    [:div
     [:input.input attrs]
     (when-not valid?
       [:p.help.is-danger message])]))

(defmethod input-view "textarea"
  [form {:keys [key type disabled read-only rows value] :as field}]
  (let [[valid? message] (validate form field)
        attrs            {:type        type
                          :name        (field-name field)
                          :placeholder (field-name field)
                          :class       (when-not valid? "is-danger")
                          :value       (get @form key value)
                          :on-change   (field-handler form key)
                          :read-only   read-only
                          :rows        rows
                          :disabled    disabled}]
    [:div
     [:textarea.textarea attrs]
     (when-not valid?
       [:p.help.is-danger message])]))

(defmethod input-view "code"
  [form {:keys [value]}]
  [:pre [:code value]])

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



(defn input-label
  [field]
  [:label.label [:b (field-name field) (when (:required field) "*") " "]])

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
  [form fields]
  [:div
   (for [{:keys [key] :as field} fields]
     ^{:key key}[field-view form field])
   [:div]]) ;; need this div to force a margin below last field

(defn cancel-button
  [{:keys [on-cancel]}]
  (when on-cancel
    [:div.control
     [:button.button.is-medium {:type     "button"
                                :on-click #(re-frame/dispatch on-cancel)}
      "Cancel"]]))

(defn submit-button
  [form {:keys [on-submit submit-text fields submit-class]}]
  (let [valid? @(re-frame/subscribe [:valid-form? @form fields])
        class  (str submit-class " " (when-not valid? "is-static"))]
    [:button.button.is-primary.is-medium
     {:type     "submit"
      :class    class
      :on-click (fn [event]
                  (re-frame/dispatch (conj on-submit @form))
                  (.preventDefault event))}
     submit-text]))

(defn form-view
  "Generate the hiccup of a form based on a spec map, with internally managed
  state."
  [{:keys [fields defaults] :or {defaults {}} :as spec}]
  (let [form (reagent/atom defaults)]
    (fn []
      [:form
       [detached-form-view form fields]
       [:div.field.is-grouped.is-grouped-centered
        [cancel-button spec]
        [submit-button form spec]]])))
