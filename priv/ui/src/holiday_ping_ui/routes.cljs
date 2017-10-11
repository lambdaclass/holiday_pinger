(ns holiday-ping-ui.routes
  (:require [bidi.bidi :as bidi]
            [pushy.core :as pushy]
            [re-frame.core :as re-frame]))

;; maybe slugify names instead of having to put a regex here?
(def app-routes ["/" [[""                                         :landing]
                      ["channels/new/"                            :channel-create]
                      [["channels/" [#".+" :channel] "/edit"]     :channel-edit]
                      [["channels/" [#".+" :channel] "/holidays"] :holidays]
                      ["login"                                    :login]
                      ["register"                                 :register]
                      ["register/confirm/code"                    :register-confirm]
                      ["register/confirm"                         :resend-confirmation]
                      ["password"                                 :request-password-reset]
                      ["password/code"                            :submit-password-reset]
                      ["oauth/github/callback"                    :github-callback]
                      [true                                       :not-found]]])

(defn parse-url [url]
  (bidi/match-route app-routes url))

(defn- dispatch-route [matched-route]
  (let [view      (:handler matched-route)
        ;; FIXME this wont work with multiple arguments, views should get a map instead of a vector of args
        view-args (vals (:route-params matched-route))]
    (re-frame/dispatch (apply vector :switch-view view view-args))))

(def history
  (pushy/pushy dispatch-route parse-url))

(defn start-history!
  []
  (pushy/start! history))

(def url-for (partial bidi/path-for app-routes))

(defn set-history!
  [view & args]
  (pushy/set-token! history (apply url-for view args)))

(defn auth-route?
  "Return true if the given route is intended for unauthorized users
  (loging, register, etc.)."
  [view]
  (contains?
   #{:landing :login :register :github-callback :not-verified
     :register-confirm :email-sent :register-confirm-error :resend-confirmation
     :request-password-reset :submit-password-reset :password-reset-sent}
   view))
