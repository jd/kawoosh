(defpackage kawoosh.dao
  (:use cl
        cl-json
        postmodern)
  (:export dao-object
           drop-tables
           create-tables
           *dbname*
           *dbuser*
           *dbpassword*
           *dbhost*
           get-log-entry-for-user
           get-log-entries-for-user+server
           with-pg-connection
           user
           server
           connection
           channel
           log-connection
           log-entry
           id
           user-name
           user-password
           server-name
           server-address
           server-port
           server-ssl-p
           connection-id
           connection-server
           connection-username
           connection-nickname
           connection-current-nickname
           connection-realname
           connection-connected-p
           connection-motd
           connection-network-connection
           channel-connection
           channel-name
           channel-password
           channel-joined-at
           channel-names
           channel-modes
           channel-topic
           channel-topic-who
           channel-topic-time
           channel-creation-time
           user-has-access-p
           log-id
           log-reply
           log-command
           log-error))

(in-package :kawoosh.dao)


(defparameter *sql-readtable* (cl-postgres:copy-sql-readtable cl-postgres:*sql-readtable*)
  "Our SQL read table.
Copied because we want to use local-time as timestamp reader.")

;; Install local-time timestamp reader
(let ((cl-postgres:*sql-readtable* *sql-readtable*))
  (local-time:set-local-time-cl-postgres-readers))

(defparameter *dbname* "kawoosh"
  "Database name.")

(defparameter *dbuser* "kawoosh"
  "Databaser user name.")

(defparameter *dbpassword* "kawoosh"
  "Databaser password.")

(defparameter *dbhost* "localhost"
  "Databaser host.")

(defmacro with-pg-connection (&rest body)
  `(let ((cl-postgres:*sql-readtable* *sql-readtable*))
     (with-connection (list *dbname* *dbuser* *dbpassword* *dbhost*)
       ,@body)))

(defmethod encode-json ((o local-time:timestamp)
                        &optional (stream *json-output*))
  "Write the JSON representation (Object) of the SIMPLE-DATE:TIMESTAMP object
O to STREAM (or to *JSON-OUTPUT*)."
  (encode-json
   (with-output-to-string (s)
     (local-time:format-timestring s o)
     s)
   stream))

(defclass dao-object () nil)

(defvar *dao-json-filter*
  '((kawoosh.dao:user password admin)
    (kawoosh.dao:connection id)
    (kawoosh.dao:log-entry connection)
    (kawoosh.dao:channel connection))
  "Fields to not export when dumping a DAO object to JSON.")

(defmethod encode-json ((o dao-object)
                        &optional (stream *json-output*))
  "Write the JSON representation (Object) of the postmodern DAO CLOS object
O to STREAM (or to *JSON-OUTPUT*)."
  (with-object (stream)
    (json::map-slots (lambda (key value)
                       (unless (loop for (class . slots) in *dao-json-filter*
                                     if (and (subtypep (class-of o) class)
                                             (member key slots))
                                       return t)
                         (as-object-member (key stream)
                           (encode-json (if (eq value :null) nil value) stream))))
                     o)))

(defclass user (dao-object)
  ((name :col-type string :initarg :name :accessor user-name)
   (password :col-type string :initarg :password :accessor user-password)
   (admin :col-type boolean :initarg :admin-p :accessor user-admin-p))
  (:metaclass postmodern:dao-class)
  (:table-name users)
  (:keys name))

(defgeneric user-has-access-p (user access &optional accessed-user)
  (:documentation "Check that USER has ACCESS level."))

(defmethod user-has-access-p ((user user) access &optional accessed-user)
  (or (string= access 'any)
      (user-admin-p user)
      (and (string= access 'user)
           (string= (user-name user) accessed-user))))

(defclass server (dao-object)
  ((name :col-type string :initarg :name :accessor server-name)
   (address :col-type string :initarg :address :accessor server-address)
   (port :col-type integer :initarg :port :accessor server-port)
   (ssl :col-type boolean :initarg :ssl :accessor server-ssl-p))
  (:metaclass postmodern:dao-class)
  (:table-name servers)
  (:keys name))

(defclass connection (dao-object)
  ((id :col-type serial :reader connection-id)
   (server :col-type text :initarg :server :accessor connection-server)
   (username :col-type string :initarg :username :accessor connection-username)
   (nickname :col-type string :initarg :nickname :accessor connection-nickname)
   (current-nickname :col-type string :initarg :nickname :accessor connection-current-nickname)
   (realname :col-type string :initarg :nickname :accessor connection-realname)
   (connected :col-type boolean :accessor connection-connected-p)
   (motd :col-type text :accessor connection-motd)
   (network-connection :initform nil :accessor connection-network-connection))
  (:metaclass postmodern:dao-class)
  (:keys id))

(defclass channel (dao-object)
  ((connection :col-type serial :initarg :connection :accessor channel-connection)
   (name :col-type text :initarg :name :accessor channel-name)
   (password :col-type text :accessor channel-password)
   (names :col-type text[] :accessor channel-names)
   (modes :col-type text[] :accessor channel-modes)
   (joined-at :col-type timestamp :initarg :joined-at :accessor channel-joined-at)
   (topic :col-type text :accessor channel-topic)
   (topic-who :col-type text :accessor channel-topic-who)
   (topic-time :col-type timestamp :accessor channel-topic-time)
   (creation-time :col-type timestamp :accessor channel-creation-time))
  (:metaclass postmodern:dao-class)
  (:table-name channels)
  (:keys connection name))

(defclass log-entry (dao-object)
  ((id :col-type serial :reader log-id)
   (connection :col-type serial :initarg :connection :accessor log-connection)
   (time :col-type timestamp :initarg :time :accessor log-time)
   (source :col-type text :initarg :source :access log-source)
   (command :col-type text :initarg :command :access log-command)
   (target :col-type text :initarg :target :access log-target)
   (payload :col-type text :initarg :payload :access log-payload))
  (:metaclass dao-class)
  (:table-name logs)
  (:keys id))

(defun get-log-entry-for-user (username log-id)
  "Retrieve log entry with LOG-ID that belongs to USERNAME."
  (car
   (select-dao 'log-entry
       (:and
        (:= 'id log-id)
        (:in 'connection
             (:select 'id :from 'connection
              :where (:= 'username username)))))))

(defun get-log-entries-for-user+server (username server
                                        &key
                                          (class 'log-entry)
                                          from)
  "Return log for USERNAME and SERVER created after MIN-ID."
  (select-dao class
      (:and
       (:>= 'time (or from "-infinity"))
       (:in 'connection
            (:select 'id :from 'connection
             :where (:and (:= 'username username)
                          (:= 'server server)))))
      'id))

(defclass log-reply (log-entry)
  ()
  (:metaclass dao-class)
  (:table-name reply)
  (:keys id))

(defclass log-command (log-entry)
  ()
  (:metaclass dao-class)
  (:table-name command)
  (:keys id))

(defclass log-error (log-entry)
  ()
  (:metaclass dao-class)
  (:table-name error)
  (:keys id))

(defun drop-tables ()
  (with-pg-connection
    (execute "SET client_min_messages = 'ERROR';")
    (dolist (table-name '(command reply error logs channels connection servers users))
      (execute (:drop-table :if-exists table-name)))))

(defun create-tables ()
  (with-pg-connection
    (execute "SET client_min_messages = 'ERROR';")
    (execute "CREATE TABLE users (
       name text NOT NULL PRIMARY KEY CHECK (name SIMILAR TO '[a-zA-Z0-9]+'),
       password text,
       admin boolean NOT NULL DEFAULT FALSE
);")
    (execute "CREATE TABLE servers (
       name text PRIMARY KEY CHECK (name SIMILAR TO '[a-zA-Z0-9]+'),
       address text NOT NULL CHECK (address ~* E'^(([a-z0-9]|[a-z0-9][a-z0-9-]*[a-z0-9])\\.)*([a-z]|[a-z][a-z0-9-]*[a-z0-9])$'),
       port integer DEFAULT 6667 CHECK (port > 0 AND port < 65536),
       ssl boolean NOT NULL DEFAULT FALSE
);")
    (execute "CREATE TABLE connection (
       id serial PRIMARY KEY,
       server text REFERENCES servers(name),
       username text NOT NULL REFERENCES users(name) ON DELETE CASCADE,
       nickname text NOT NULL CHECK (nickname SIMILAR TO '[a-zA-Z][a-zA-Z0-9\\-_\\[\\]\\\\`{}]+'),
       current_nickname text,
       realname text,
       motd text,
       connected boolean NOT NULL DEFAULT FALSE,
       UNIQUE (server, username)
);")
    (execute "CREATE TABLE channels (
	connection serial NOT NULL REFERENCES connection(id) ON DELETE CASCADE,
	name varchar(50) NOT NULL CONSTRAINT rfc2812 CHECK (name ~ E'^[!#&+][^ ,\\x07\\x13\\x10]'),
        -- XXX update password when modes is updated for password
	password text,
        joined_at timestamp with time zone,
        names text[],
        modes text[],
        topic text,
        topic_who text,
        topic_time timestamp with time zone,
        creation_time timestamp with time zone,
	PRIMARY KEY (connection, name)
);")
    (execute "CREATE TABLE logs (
	id serial PRIMARY KEY,
	connection serial NOT NULL REFERENCES connection(id) ON DELETE CASCADE,
	time timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
	source text NOT NULL,
	command text NOT NULL,
	target text NOT NULL,
	payload text
);")
    (execute "CREATE TABLE reply (
	connection serial NOT NULL REFERENCES connection(id) ON DELETE CASCADE
) INHERITS (logs);")
    (execute "ALTER TABLE reply ADD PRIMARY KEY (id);")
    (execute "CREATE TABLE error (
	connection serial NOT NULL REFERENCES connection(id) ON DELETE CASCADE
) INHERITS (logs);")
    (execute "ALTER TABLE error ADD PRIMARY KEY (id);")
    (execute "CREATE TABLE command (
	connection serial NOT NULL REFERENCES connection(id) ON DELETE CASCADE
) INHERITS (logs);")
    (execute "ALTER TABLE command ADD PRIMARY KEY (id);")
    ;; Lower and trim channels.name on insertion
    (execute "CREATE OR REPLACE FUNCTION lower_name() RETURNS trigger AS $lower_name$
BEGIN
  NEW.name := lower(trim(NEW.name));
  RETURN NEW;
END;
$lower_name$
LANGUAGE plpgsql;")
    (execute "CREATE TRIGGER lower_name BEFORE INSERT ON channels FOR EACH ROW EXECUTE PROCEDURE lower_name();")
    (execute "CREATE OR REPLACE FUNCTION lower_address() RETURNS trigger AS $lower_address$
BEGIN
  NEW.address := lower(trim(NEW.address));
  RETURN NEW;
END;
$lower_address$
LANGUAGE plpgsql;")
    (execute "CREATE TRIGGER lower_address BEFORE INSERT ON servers FOR EACH ROW EXECUTE PROCEDURE lower_address();")
    (execute "CREATE OR REPLACE FUNCTION notify_on_insert() RETURNS trigger AS $$
BEGIN
  IF (TG_OP = 'INSERT') THEN
    PERFORM pg_notify('log_inserted_for_connection_' || NEW.connection,
                       '((id . ' || NEW.id || ')'
                        '(time . \"' || NEW.time || '\")'
                        '(source . \"' || NEW.source || '\")'
                        '(command . \"' || NEW.command || '\")'
                        '(target . \"' || NEW.target || '\")'
                        '(payload . \"' || replace(NEW.payload, '\"\', '\\\"') || '\"))');
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;")
    (execute "CREATE TRIGGER reply_notify_on_insert AFTER INSERT ON reply FOR EACH ROW EXECUTE PROCEDURE notify_on_insert();")
    (execute "CREATE TRIGGER error_notify_on_insert AFTER INSERT ON error FOR EACH ROW EXECUTE PROCEDURE notify_on_insert();")
    (execute "CREATE TRIGGER command_notify_on_insert AFTER INSERT ON command FOR EACH ROW EXECUTE PROCEDURE notify_on_insert();")
    (execute "INSERT INTO users (name, password, admin) VALUES ('admin', 'admin', true);")))
