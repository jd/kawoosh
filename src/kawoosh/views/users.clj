(ns kawoosh.views.users
  (:use [kawoosh.models.users :as users])
  (:require noir.response)
  (:use [noir.core :only [defpage]]))

(defpage "/users" {}
  (noir.response/json @(users/list)))

(defpage [:post "/users/:name"] {name :name}
  (users/create name)
  (noir.response/json {:name name}))

(defpage [:delete "/users/:name"] {name :name}
  (users/delete name)
  (noir.response/empty))

(defpage "/users/:user" {user :user}
  (noir.response/json (first @(users/get user))))

;; (defpage "/users/:user/networks" {}
;;   (noir.response/json (get (get users user) :networks)))

;; (defpage [:post "/users/:user/networks/:network"] {user :user network :network}
;;   (noir.response/json (get users user)))

;; (defpage "/users/:user" {user :user}
;;   (noir.response/json (get users user)))


