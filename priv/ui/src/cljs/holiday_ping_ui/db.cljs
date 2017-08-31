(ns holiday-ping-ui.db
  (:require [cljs-time.core :as time]))

(def default-db
  {:current-view           :login
   :current-view-args      []
   :error-message          nil
   :success-message        nil
   :access-token           nil
   :holidays-saved         nil
   :holidays-edited        nil
   :channels               nil
   :channel-to-test        nil
   :calendar-selected-year (time/year (time/today))})
