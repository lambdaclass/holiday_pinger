(ns holiday-ping-ui.auth.views
  (:require
   [re-frame.core :as re-frame]
   [holiday-ping-ui.common.views :as views]
   [holiday-ping-ui.common.forms :as forms]
   [holiday-ping-ui.auth.countries :as countries]))

;;; AUTH VIEWS

(defn login-view []
  [:div
   [views/section-size :is-one-third
    [:div.card
     [:div.card-content
      [:div.has-text-centered
       [:a.button.is-medium.is-primary.is-fullwidth
        {:href "/oauth/github"}
        [:span.icon.is-medium [:i.fa.fa-github]]
        [:span
         " Login with GitHub"]]]
      [:hr]
      [views/message-view]
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
       [:a {:href "#" :on-click #(re-frame/dispatch [:switch-view :register])} "Click here to register."]]]]]])

(defn register-view []
  (let [user-country @(re-frame/subscribe [:country])]
    [:div
     [views/section-size :is-half
      [:p.subtitle "Please fill your profile information."]
      [views/message-view]
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
       [:a {:href "#" :on-click #(re-frame/dispatch [:switch-view :login])} "Click here to login."]]]]))

(defn github-loading-view
  []
  [:div
   [views/section-size :is-one-third
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
    [views/section
     [:p.subtitle "Please fill your profile information."]
     [views/message-view]
     [forms/form-view {:submit-text "Register"
                       :on-submit   [:github-register-submit]
                       :fields      [{:key       :country
                                      :type      "select"
                                      :options   countries/list
                                      :value     user-country
                                      :help-text "We'll use this to load you default holidays."
                                      :required  true}]}]]))
