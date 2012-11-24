(ns kawoosh.models.networks
  (:use clojureql.core)
  (:require [kawoosh.models.db :as db]))

(defn create [user name]
  (conj! (table db/db :users)
         {:name name}))

(defn get [name]
  (select (project (table db/db :users) [:name])
          (where (= :name name))))

(defn list []
  (project (table db/db :users) [:name]))

(defn delete [name]
  (disj! (table db/db :users) (where (= :name name))))