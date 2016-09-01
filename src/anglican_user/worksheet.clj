(ns anglican-user.worksheet
  (:require [gorilla-plot.core :as plot])
  (:use [anglican core emit runtime
         [state :only [get-predicts get-log-weight]]]))
