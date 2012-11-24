(ns kawoosh.views.networks
  (:use [kawoosh.models.networks :as networks])
  (:require noir.response)
  (:use [noir.core :only [defpage]]))

(defpage "/users/:user/networks" {user :user}
  (noir.response/json @(networks/list user)))

(defpage [:post "/users/:user/networks/:network"] {user :user network :network}
  (networks/create user network)
  (noir.response/json {:name network}))

(defpage [:delete "/users/:user/networks/:network"] {user :user network :network}
  (networks/delete user network)
  (noir.response/empty))

(defpage "/users/:user/networks/:network" {user :user network :network}
  (noir.response/json (first @(networks/get user network))))
