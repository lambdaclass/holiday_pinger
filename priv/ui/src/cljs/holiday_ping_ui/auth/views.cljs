(ns holiday-ping-ui.auth.views
  (:require
   [holiday-ping-ui.routes :as routes]
   [holiday-ping-ui.common.views :as views]
   [holiday-ping-ui.common.forms :as forms]))

;;; AUTH VIEWS

(defn login-view []
  [:div
   [views/section-size :is-one-third
    [:div.card
     [:div.card-content
      [:div.has-text-centered
       [:a.button.is-medium.is-primary.is-fullwidth
        {:data-pushy-ignore true ;; don't try to hadle this uri in the frontend
         :href              "/oauth/github"}
        [:span.icon.is-medium [:i.fa.fa-github]]
        [:span " Login with GitHub"]]]
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
       [:a {:href (routes/url-for :register)} "Click here to register."]]]]]])

(defn register-view []
  [:div
   [views/section-size :is-half
    [:p.subtitle "Please fill your profile information."]
    [views/message-view]
    [forms/form-view {:submit-text "Register"
                      :on-submit   [:register-submit]
                      :fields      [{:key      :email
                                     :type     "email"
                                     :validate :valid-email?
                                     :required true}
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
                                     :validate :matching-passwords?
                                     :required true}]}]
    [:br]
    [:p.has-text-centered "Already registered? "
     [:a {:href (routes/url-for :login)} "Click here to login."]]]])
