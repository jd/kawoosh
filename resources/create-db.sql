-- Domains
-- CREATE DOMAIN fqdn AS text CHECK (value ~* E'^(([a-z0-9]|[a-z0-9][a-z0-9-]*[a-z0-9])\.)*([a-z]|[a-z][a-z0-9-]*[a-z0-9])$');


-- Tables
CREATE TABLE users (
       name text NOT NULL PRIMARY KEY CHECK (name SIMILAR TO '[a-zA-Z0-9]+'),
       password text
);

CREATE TABLE servers (
       id serial PRIMARY KEY,
       name text NOT NULL,
       address text NOT NULL CHECK (address ~* E'^(([a-z0-9]|[a-z0-9][a-z0-9-]*[a-z0-9])\.)*([a-z]|[a-z][a-z0-9-]*[a-z0-9])$'),
       port integer DEFAULT 6667 CHECK (port > 0 AND port < 65536),
       username text NOT NULL REFERENCES users(name) ON DELETE CASCADE,
       nickname text NOT NULL CHECK (nickname SIMILAR TO '[a-zA-Z][a-zA-Z0-9\-_\[\]\\`{}]+'),
       realname text,
       UNIQUE (name, username)
);

CREATE TABLE channels (
	id serial PRIMARY KEY,
	server serial NOT NULL REFERENCES servers(id) ON DELETE CASCADE,
	name varchar(50) NOT NULL CONSTRAINT rfc2812 CHECK (name ~ E'^[!#&+][^ ,\x07\x13\x10]'),
	password text,
	UNIQUE (server, name)
);

CREATE TABLE logs (
	id serial PRIMARY KEY,
	server serial NOT NULL REFERENCES servers(id) ON DELETE CASCADE,
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
CREATE OR REPLACE FUNCTION channels_notify() RETURNS trigger AS $$
BEGIN
  IF (TG_OP = 'DELETE') THEN
    PERFORM pg_notify('channel_' || OLD.server, '');
  ELSIF (TG_OP = 'UPDATE') THEN
    PERFORM pg_notify('channel_' || NEW.server, '');
    IF (OLD.server != NEW.server) THEN
      PERFORM pg_notify('channel_' || NEW.server, '');
    END IF;
  ELSIF (TG_OP = 'INSERT') THEN
    PERFORM pg_notify('channel_' || NEW.server, '');
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER channels_notify_insert AFTER INSERT OR UPDATE OR DELETE ON channels FOR EACH ROW EXECUTE PROCEDURE channels_notify();

-- Basic data
INSERT INTO users (name) VALUES ('jd');
WITH net AS (
     INSERT INTO servers (name, username, address, port, nickname) VALUES ('Naquadah', 'jd', 'orion', 8067, 'jdk') RETURNING id
) INSERT INTO channels (server, name) SELECT id, '#test' FROM net;
INSERT INTO channels (server, name) SELECT id, '#test-bis' FROM servers WHERE username='jd' AND name='Naquadah';

