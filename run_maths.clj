(ns symbolic-maths.core
  (:use noir.core)
  (:require [noir.server :as server]))

(defpage "/welcome" []
    "Welcome to Noir!")

(server/start 8080)
