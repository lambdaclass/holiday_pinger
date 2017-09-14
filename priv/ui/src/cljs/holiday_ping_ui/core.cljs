(ns holiday-ping-ui.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]

            [holiday-ping-ui.common.events]
            [holiday-ping-ui.auth.events]
            [holiday-ping-ui.channels.events]
            [holiday-ping-ui.holidays.events]

            [holiday-ping-ui.common.subs]
            [holiday-ping-ui.auth.subs]
            [holiday-ping-ui.channels.subs]
            [holiday-ping-ui.holidays.subs]

            [holiday-ping-ui.common.views :as common]
            [holiday-ping-ui.auth.views :as auth]
            [holiday-ping-ui.channels.views :as channels]
            [holiday-ping-ui.holidays.views :as holidays]

            [holiday-ping-ui.config :as config]))

(def views {:channel-list    [channels/list-view]
            :channel-edit    [channels/edit-view]
            :channel-create  [channels/add-view]
            :login           [auth/login-view]
            :register        [auth/register-view]
            :github-loading  [auth/github-loading-view]
            :github-register [auth/github-register-view]
            :holidays        [holidays/holidays-view]})

(defn app
  "Build the ui based on the current-view in the app-db."
  []
  (let [current-view      @(re-frame/subscribe [:current-view])
        current-view-args @(re-frame/subscribe [:current-view-args])
        [view]            (get views current-view)]
    [:div
     [common/navbar-view]
     (apply vector view current-view-args)
     [common/footer-view]]))

(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [app]
                  (.getElementById js/document "app")))

(defn ^:export init []
  (dev-setup)
  (re-frame/dispatch-sync [:initialize-db])
  (mount-root))
