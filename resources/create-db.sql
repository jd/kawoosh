-- Domains
-- CREATE DOMAIN fqdn AS text CHECK (value ~* E'^(([a-z0-9]|[a-z0-9][a-z0-9-]*[a-z0-9])\.)*([a-z]|[a-z][a-z0-9-]*[a-z0-9])$');


-- Tables
CREATE TABLE users (
       name text NOT NULL PRIMARY KEY CHECK (name SIMILAR TO '[a-zA-Z0-9]+'),
       password text
);

CREATE TABLE networks (
       name text PRIMARY KEY CHECK (name SIMILAR TO '[a-zA-Z0-9.\_\-\[\]\{\}]+')
);

-- XXX Check on INSERT/UPDATE that network.username == NULL or network.username == username
CREATE TABLE network_connections (
       id serial PRIMARY KEY,
       username text NOT NULL REFERENCES users(name) ON DELETE CASCADE,
       network text NOT NULL REFERENCES networks(name) ON DELETE CASCADE,
       nickname text NOT NULL CHECK (nickname SIMILAR TO '[a-zA-Z][a-zA-Z0-9\-_\[\]\\`{}]+'),
       realname text,
       worker_ping timestamp,
       UNIQUE (network, username)
);

CREATE TABLE servers (
	address text PRIMARY KEY CHECK (address ~* E'^(([a-z0-9]|[a-z0-9][a-z0-9-]*[a-z0-9])\.)*([a-z]|[a-z][a-z0-9-]*[a-z0-9])$'),
	port integer DEFAULT 6667 CHECK (port > 0 AND port < 65536),
	network text NOT NULL REFERENCES networks(name) ON DELETE CASCADE
);

CREATE TABLE channels (
	id serial PRIMARY KEY,
	network_connection serial NOT NULL REFERENCES network_connections(id) ON DELETE CASCADE,
	name varchar(50) NOT NULL CONSTRAINT rfc2812 CHECK (name ~ E'^[!#&+][^ ,\x07\x13\x10]'),
	password text,
	UNIQUE (network_connection, name)
);

CREATE TABLE logs (
	id serial PRIMARY KEY,
	network_connection serial NOT NULL REFERENCES network_connections(id) ON DELETE CASCADE,
	time timestamp NOT NULL DEFAULT CURRENT_DATE,
	source text NOT NULL,
	command text NOT NULL,
	target text NOT NULL,
	payload text NOT NULL
);


-- Functions/triggers
--- Lower and trim channels.name on insertion
CREATE OR REPLACE FUNCTION lower_name() RETURNS trigger AS $lower_name$
BEGIN
  NEW.name := lower(trim(NEW.name));
  RETURN NEW;
END;
$lower_name$
LANGUAGE plpgsql;

CREATE TRIGGER lower_name BEFORE INSERT ON channels FOR EACH ROW EXECUTE PROCEDURE lower_name();

--- Lower and trim server.address.name on insertion
CREATE OR REPLACE FUNCTION lower_address() RETURNS trigger AS $lower_address$
BEGIN
  NEW.address := lower(trim(NEW.address));
  RETURN NEW;
END;
$lower_address$
LANGUAGE plpgsql;

CREATE TRIGGER lower_address BEFORE INSERT ON servers FOR EACH ROW EXECUTE PROCEDURE lower_address();

--- Notify on new channels
-- CREATE OR REPLACE FUNCTION channels_notify() AS $$
-- NOTIFY NEW

-- CREATE TRIGGER channels_notify BEFORE INSERT ON channels FOR EACH STATEMENT EXECUTE PROCEDURE channels_notify();

-- Basic data
INSERT INTO users (name) VALUES ('jd');
INSERT INTO networks VALUES ('Naquadah');
INSERT INTO servers VALUES ('orion', 8067, 'Naquadah');
WITH net AS (
     INSERT INTO network_connections (username, network, nickname) VALUES ('jd', 'Naquadah', 'jdk') RETURNING id
) INSERT INTO channels (network_connection, name) SELECT id, '#test' FROM net;
INSERT INTO channels (network_connection, name) SELECT id, '#test-bis' FROM network_connections WHERE username='jd' AND network='Naquadah';

