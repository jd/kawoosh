(defpackage kawoosh.dao
  (:use cl
        postmodern)
  (:export dao-object
           user
           server
           connection
           channel
           password
           id))

(in-package :kawoosh.dao)

(defclass dao-object () nil)

(defclass user (dao-object)
  ((name :col-type string :initarg :name :accessor user-name)
   (password :col-type string :initarg :password :accessor user-password))
  (:metaclass postmodern:dao-class)
  (:table-name users)
  (:keys name))

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
   (current_nickname :col-type string :initarg :nickname :accessor connection-current-nickname)
   (realname :col-type string :initarg :nickname :accessor connection-realname)
   (connected :col-type boolean :accessor connection-connected-p)
   (motd :col-type text :accessor connection-motd)
   (network-connection :initform nil :accessor connection-network-connection))
  (:metaclass postmodern:dao-class)
  (:keys id))

(defclass channel (dao-object)
  ((id :col-type serial :reader channel-id)
   (connection :col-type serial :accessor channel-connection)
   (name :col-type text :accessor channel-name)
   (password :col-type text :accessor channel-password)
   (names :col-type text[] :accessor channel-names)
   (modes :col-type text[] :accessor channel-modes)
   (topic :col-type text :accessor channel-topic)
   (topic_who :col-type text :accessor channel-topic-who)
   (topic_time :col-type timestamp :accessor channel-topic-time)
   (creation_time :col-type timestamp :accessor channel-creation-time))
  (:metaclass postmodern:dao-class)
  (:table-name channels)
  (:keys id))
