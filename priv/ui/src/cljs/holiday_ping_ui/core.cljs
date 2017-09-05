(ns holiday-ping-ui.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [holiday-ping-ui.events]
            [holiday-ping-ui.subs]
            [holiday-ping-ui.views :as views]
            [holiday-ping-ui.config :as config]))


(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views/app]
                  (.getElementById js/document "app")))

(defn ^:export init []
  (dev-setup)
  (re-frame/dispatch-sync [:initialize-db])
  (mount-root))
