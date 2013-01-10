-- Tables
CREATE TABLE users (
       name text NOT NULL PRIMARY KEY CHECK (name SIMILAR TO '[a-zA-Z0-9]+'),
       password text
);

CREATE TABLE servers (
       name text PRIMARY KEY CHECK (name SIMILAR TO '[a-zA-Z0-9]+'),
       address text NOT NULL CHECK (address ~* E'^(([a-z0-9]|[a-z0-9][a-z0-9-]*[a-z0-9])\.)*([a-z]|[a-z][a-z0-9-]*[a-z0-9])$'),
       port integer DEFAULT 6667 CHECK (port > 0 AND port < 65536),
       ssl boolean NOT NULL DEFAULT FALSE
);

CREATE TABLE connection (
       id serial PRIMARY KEY,
       server text REFERENCES servers(name),
       username text NOT NULL REFERENCES users(name) ON DELETE CASCADE,
       nickname text NOT NULL CHECK (nickname SIMILAR TO '[a-zA-Z][a-zA-Z0-9\-_\[\]\\`{}]+'),
       current_nickname text,
       realname text,
       motd text,
       connected boolean NOT NULL DEFAULT FALSE,
       UNIQUE (server, username)
);

CREATE TABLE channels (
	id serial PRIMARY KEY,
	connection serial NOT NULL REFERENCES connection(id) ON DELETE CASCADE,
	name varchar(50) NOT NULL CONSTRAINT rfc2812 CHECK (name ~ E'^[!#&+][^ ,\x07\x13\x10]'),
	password text,
        names text[],
        modes text[],
        topic text,
        topic_who text,
        topic_time timestamp,
        creation_time timestamp,
	UNIQUE (connection, name)
);

CREATE TABLE logs (
	id serial PRIMARY KEY,
	connection serial NOT NULL REFERENCES connection(id) ON DELETE CASCADE,
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
    PERFORM pg_notify('channel_' || OLD.connection, '');
  ELSIF (TG_OP = 'UPDATE') THEN
    IF (OLD.connection != NEW.connection) OR (OLD.name != NEW.name) THEN
      RAISE EXCEPTION 'You are not allowed to modify name or connection of a channel'
            USING HINT = 'Do DELETE and then INSERT';
    ELSIF (OLD.password != NEW.password) THEN
      -- Password change, re-join channel
      PERFORM pg_notify('channel_' || NEW.connection, '');
    END IF;
  ELSIF (TG_OP = 'INSERT') THEN
    PERFORM pg_notify('channel_' || NEW.connection, '');
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER channels_notify_insert AFTER INSERT OR UPDATE OR DELETE ON channels FOR EACH ROW EXECUTE PROCEDURE channels_notify();

-- Basic data
INSERT INTO users (name) VALUES ('jd');
INSERT INTO servers (name, address, ssl) VALUES ('Naquadah', 'irc.naquadah.org', true);
WITH conn AS (
     INSERT INTO connection (server, username, nickname) VALUES ('Naquadah', 'jd', 'jd') RETURNING id
) INSERT INTO channels (connection, name) SELECT id, '#test' FROM conn;
INSERT INTO channels (connection, name) SELECT id, '#test-bis' FROM connection WHERE username='jd' AND server='Naquadah';

