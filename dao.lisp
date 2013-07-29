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
           get-event-for-user
           get-events-for-user+server
           with-pg-connection
           user
           server
           connection
           channel
           event-connection
           event
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
           event-id
           event-reply
           event-sent
           event-error))

(in-package :kawoosh.dao)

(defmethod s-sql:sql-escape ((arg symbol))
  "Overrides the s-sql provided function so it handles correctly the :false
keyword and get it converted to false."
  (if (or (typep arg 'boolean) (eq arg :null) (eq arg :false))
      (call-next-method)
      (s-sql:to-sql-name arg)))

(defmethod cl-postgres:to-sql-string ((arg (eql nil)))
  "Overrides the cl-postgres provided function so it handles nil as NULL instead of false."
  "NULL")

(defmethod cl-postgres:to-sql-string ((arg (eql :false)))
  "Overrides the cl-postgres provided function so it handles :false."
  "false")

(defmethod cl-postgres:to-sql-string ((arg (eql 'yason:true)))
  "Overrides the cl-postgres provided function so it handles Yason true
value."
  "true")

(defmethod cl-postgres:to-sql-string ((arg (eql 'yason:false)))
  "Overrides the cl-postgres provided function so it handles Yason false value."
  "false")


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

;; TODO Move json method in kawoosh.json // json.lisp
(defmethod encode-json ((o local-time:timestamp)
                        &optional (stream *json-output*))
  "Write the JSON representation (Object) of the SIMPLE-DATE:TIMESTAMP object
O to STREAM (or to *JSON-OUTPUT*)."
  (encode-json
   (with-output-to-string (s)
     (local-time:format-timestring s o)
     s)
   stream))

(defmethod encode-json ((o (eql :false))
                        &optional (stream *json-output*))
  (json::write-json-chars "false" stream))


(defclass dao-object () nil)

;; TODO Try to make this automagically defined on boolean slots!
(defmacro dao-define-boolean-method (method-name class-name slot-name)
  `(progn
     (defmethod ,method-name ((object ,class-name))
      (with-slots (,slot-name) object
        (not (or (eq ,slot-name :false) (eq ,slot-name nil)))))

     (defmethod (setf ,method-name) (value (object ,class-name))
       (setf (slot-value object ',slot-name) (or value :false)))))

(defvar *dao-json-filter*
  '((kawoosh.dao:user password admin)
    (kawoosh.dao:connection id)
    (kawoosh.dao:event connection)
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
   (ssl :col-type boolean :initarg :ssl))
  (:metaclass postmodern:dao-class)
  (:table-name servers)
  (:keys name))

(dao-define-boolean-method server-ssl-p server ssl)

(defclass connection (dao-object)
  ((id :col-type serial :reader connection-id)
   (server :col-type text :initarg :server :accessor connection-server)
   (username :col-type string :initarg :username :accessor connection-username)
   (nickname :col-type string :initarg :nickname :accessor connection-nickname)
   (current-nickname :col-type string :initarg :nickname :accessor connection-current-nickname)
   (realname :col-type string :initarg :nickname :accessor connection-realname)
   (connected :col-type boolean)
   (motd :col-type text :accessor connection-motd)
   (network-connection :initform nil :accessor connection-network-connection))
  (:metaclass postmodern:dao-class)
  (:keys id))

(dao-define-boolean-method connection-connected-p connection connected)

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

(defclass event (dao-object)
  ((id :col-type serial :reader event-id)
   (connection :col-type serial :initarg :connection :accessor event-connection)
   (time :col-type timestamp :initarg :time :accessor event-time)
   (source :col-type text :initarg :source :access event-source)
   (command :col-type text :initarg :command :access event-command)
   (target :col-type text :initarg :target :access event-target)
   (payload :col-type text :initarg :payload :access event-payload))
  (:metaclass dao-class)
  (:keys id))

(defun get-event-for-user (username event-id)
  "Retrieve event entry with EVENT-ID that belongs to USERNAME."
  (car
   (select-dao 'event
       (:and
        (:= 'id event-id)
        (:in 'connection
             (:select 'id :from 'connection
              :where (:= 'username username)))))))

(defun get-events-for-user+server (username server
                                   &key
                                     (class 'event)
                                     from)
  "Return events for USERNAME and SERVER created after FROM timestamp."
  (select-dao class
      (:and
       (:>= 'time (or from "-infinity"))
       (:in 'connection
            (:select 'id :from 'connection
             :where (:and (:= 'username username)
                          (:= 'server server)))))
      'id))

(defclass event-reply (event)
  ()
  (:metaclass dao-class)
  (:keys id))

(defclass event-sent (event)
  ()
  (:metaclass dao-class)
  (:keys id))

(defclass event-error (event)
  ()
  (:metaclass dao-class)
  (:keys id))

(defun drop-tables ()
  (with-pg-connection
    (execute "SET client_min_messages = 'ERROR';")
    (dolist (table-name '(event-sent event-reply event-error event channels connection servers users))
      (execute (:drop-table :if-exists table-name)))))

(defun create-tables ()
  (with-pg-connection
    (execute "SET client_min_messages = 'ERROR';")
    (execute "CREATE TABLE users (
       name text NOT NULL PRIMARY KEY CHECK (name SIMILAR TO '[a-zA-Z0-9]+'),
       password text NOT NULL,
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
       server text NOT NULL REFERENCES servers(name),
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
    (execute "CREATE TABLE event (
	id serial PRIMARY KEY,
	connection serial NOT NULL REFERENCES connection(id) ON DELETE CASCADE,
	time timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
	source text NOT NULL,
	command text NOT NULL,
	target text NOT NULL,
	payload text
);")
    (execute "CREATE TABLE event_reply (
	connection serial NOT NULL REFERENCES connection(id) ON DELETE CASCADE
) INHERITS (event);")
    (execute "ALTER TABLE event_reply ADD PRIMARY KEY (id);")
    (execute "CREATE TABLE event_error (
	connection serial NOT NULL REFERENCES connection(id) ON DELETE CASCADE
) INHERITS (event);")
    (execute "ALTER TABLE event_error ADD PRIMARY KEY (id);")
    (execute "CREATE TABLE event_sent (
	connection serial NOT NULL REFERENCES connection(id) ON DELETE CASCADE
) INHERITS (event);")
    (execute "ALTER TABLE event_sent ADD PRIMARY KEY (id);")
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
     -- No text must be NULL, otherwise notification sending will concatenate to NULL
    PERFORM pg_notify('event_inserted_for_connection_' || NEW.connection,
                       '((id . ' || NEW.id || ')'
                        '(time . \"' || NEW.time || '\")'
                        '(source . \"' || NEW.source || '\")'
                        '(command . \"' || NEW.command || '\")'
                        '(target . \"' || NEW.target || '\")'
                        '(payload . \"' || replace(COALESCE(NEW.payload, ''), '\"\', '\\\"') || '\"))');
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;")
    (execute "CREATE TRIGGER event_reply_notify_on_insert AFTER INSERT ON event_reply FOR EACH ROW EXECUTE PROCEDURE notify_on_insert();")
    (execute "CREATE TRIGGER event_error_notify_on_insert AFTER INSERT ON event_error FOR EACH ROW EXECUTE PROCEDURE notify_on_insert();")
    (execute "CREATE TRIGGER event_sent_notify_on_insert AFTER INSERT ON event_sent FOR EACH ROW EXECUTE PROCEDURE notify_on_insert();")
    (execute "INSERT INTO users (name, password, admin) VALUES ('admin', 'admin', true);")))
