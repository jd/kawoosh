(ns kawoosh.models.db
  (:require [clojure.java.jdbc :as sql]))

;; TODO use a configuration file
(def db {:subprotocol "postgresql"
         :subname "//localhost/kawoosh"
         :user "kawoosh"
         :password "kawoosh"})

(defn drop-tables []
  (sql/with-connection db
    (doseq [table [:logs :channels :servers :networks :users]]
      (try
        (sql/drop-table table)
        (catch Exception e)))
    (try
      (sql/do-commands "DROP DOMAIN fqdn")
      (catch Exception e))))

(defn create-tables []
  (sql/with-connection db
    (sql/do-commands
     "CREATE DOMAIN fqdn AS text CHECK (VALUE ~* E'^(([a-z0-9]|[a-z0-9][a-z0-9-]*[a-z0-9])\\.)*([a-z]|[a-z][a-z0-9-]*[a-z0-9])$')")
    (sql/create-table :users
                      [:name :text "NOT NULL" "PRIMARY KEY" "CHECK (name SIMILAR TO '[a-zA-Z0-9]+')"]
                      [:password :text])
    (sql/create-table :networks
                      [:id :serial "PRIMARY KEY"]
                      [:username :text "NOT NULL" "REFERENCES users(name)" "ON DELETE CASCADE"]
                      [:nickname :text "NOT NULL" "CHECK (nickname SIMILAR TO '[a-zA-Z][a-zA-Z0-9\\-_\\[\\]\\\\`{}]+')"]
                      [:realname :text]
                      [:name :text "NOT NULL"]
                      ["UNIQUE" "(username, name)"])
    (sql/create-table :servers
                      [:id :serial "PRIMARY KEY"]
                      [:network :serial "NOT NULL"
                       "REFERENCES networks(id)" "ON DELETE CASCADE"]
                      [:address :fqdn "NOT NULL"]
                      [:login :text]
                      [:password :text]
                      ["UNIQUE" "(network, address)"])
    (sql/create-table :channels
                      [:id :serial "PRIMARY KEY"]
                      [:network :serial "NOT NULL"
                       "REFERENCES networks(id)" "ON DELETE CASCADE"]
                      [:name "varchar(50)" "NOT NULL"
                       "CONSTRAINT rfc2812 CHECK (name ~ E'^[!#&+][^ ,\\x07\\x13\\x10]')"]
                      [:password :text]
                      ["UNIQUE" "(network, name)"])
    (sql/do-commands
     "CREATE OR REPLACE FUNCTION lower_name() RETURNS trigger AS $lower_name$
BEGIN
  NEW.name := lower(NEW.name);
  RETURN NEW;
END;
$lower_name$
LANGUAGE plpgsql")
    (sql/do-commands
     "CREATE TRIGGER lower_name BEFORE INSERT ON channels FOR EACH ROW EXECUTE PROCEDURE lower_name()")
    (sql/create-table :logs
                      [:id :serial "PRIMARY KEY"]
                      [:network :serial "NOT NULL"
                      "REFERENCES networks(id)"
                      "ON DELETE CASCADE"]
                      [:time :timestamp "NOT NULL"] ; XXX default now()
                      [:source :text "NOT NULL"]
                      [:command :text "NOT NULL"]
                      [:target :text "NOT NULL"]
                      [:payload :text "NOT NULL"])))
