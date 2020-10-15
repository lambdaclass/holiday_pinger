(ns holiday-ping-ui.auth.views
  (:require
   [holiday-ping-ui.routes :as routes]
   [holiday-ping-ui.common.views :as views]
   [holiday-ping-ui.common.forms :as forms]))

;;; AUTH VIEWS

(defn login-view []
  [:div
   [views/section-size "is-one-third-desktop is-half-tablet"
    [:div.card
     [:div.card-content
      [:div.has-text-centered
       [:a.button.is-medium.is-primary.is-fullwidth
        {:data-pushy-ignore true ;; don't try to hadle this uri in the frontend
         :href              "/oauth/github"}
        [:span.icon.is-medium [:i.fa.fa-github]]
        [:span " Login with GitHub"]]]

      [:div.has-text-centered
       [:a.button.is-medium.is-primary.is-fullwidth
        {:data-pushy-ignore true ;; don't try to hadle this uri in the frontend
         :href              "/oauth/google"}
        [:span.icon.is-medium [:i.fa.fa-github]]
        [:span " Login with Google"]]]
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
      [:p.has-text-centered
       [:a {:href (routes/url-for :request-password-reset)} "Forgot your password?"]]
      [:p.has-text-centered
       [:a {:href (routes/url-for :register)} "Don't have an account?"]]]]]])

(defn register-view []
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
    [:a {:href (routes/url-for :login)} "Click here to login."]]])

(defn auth-message-view
  [subtitle & views]
  [views/section-size :is-two-thirds
   [:p.subtitle subtitle]
   (apply vector :div.container views)])

;; EMAIL VERIFICATION VIEWS
(defn email-sent-view
  []
  [auth-message-view "Email verification sent."
   [:p "We just sent a confirmation link to the address you provided, please check your email to finish the registration process."]])

(defn register-confirm
  []
  [auth-message-view "Email verified."
   [:p "Your account has been verified, "
    [:a {:href (routes/url-for :login)} "click here to login."]]])

(defn register-confirm-error
  []
  [auth-message-view "Verification error."
   [:p "This verification link is wrong or expired, "
    [:a {:href (routes/url-for :resend-confirmation)}
     "click here to send the verification again."]]])

(defn not-verified-view
  []
  [auth-message-view "Email not verified."
   [:p "You need to verify your email address before signing in. Check the verification link sent to your email."]
   [:br]
   [:p "If you didn't receive a verification email, "
    [:a {:href (routes/url-for :resend-confirmation)} "Click here to send it again."]]])

(defn resend-confirmation-view
  []
  [views/section-size :is-half
   [:p.subtitle.has-text-centered "Enter your address for email verification."]
   [views/message-view]
   [forms/form-view {:submit-text "Send"
                     :on-submit   [:resend-confirmation-submit]
                     :fields      [{:key      :email
                                    :type     "email"
                                    :label    ""
                                    :validate :valid-email?
                                    :required true}]}]])

;; PASSWORD RESET VIEWS
(defn request-password-reset-view
  []
  [views/section-size :is-half
   [:p.subtitle.has-text-centered "Enter your email to receive a password reset link."]
   [views/message-view]
   [forms/form-view {:submit-text "Send"
                     :on-submit   [:password-reset-request]
                     :fields      [{:key      :email
                                    :type     "email"
                                    :label    ""
                                    :validate :valid-email?
                                    :required true}]}]])

(defn password-reset-sent-view
  []
  [auth-message-view "Password reset sent."
   [:p "We just sent a password reset link to your email."]])

(defn submit-password-reset-view
  []
  [views/section-size :is-half
   [:p.subtitle.has-text-centered "Enter your new password."]
   [views/message-view]
   [forms/form-view {:submit-text "Reset"
                     :on-submit   [:password-reset-submit]
                     :fields      [{:key      :password
                                    :type     "password"
                                    :required true}
                                   {:key      :password-repeat
                                    :type     "password"
                                    :label    "Repeat password"
                                    :validate :matching-passwords?
                                    :required true}]}]])

(defn password-reset-error
  []
  [auth-message-view "Password reset error."
   [:p "This password reset link is wrong or expired, "
    [:a {:href (routes/url-for :request-password-reset)}
     "click here to send it again."]]])
