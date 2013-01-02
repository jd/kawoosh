BEGIN;
SELECT plan(3);

DELETE FROM users;

INSERT INTO users (name) VALUES ('jd');
INSERT INTO servers (name, address) VALUES ('OFTC', 'irc.oftc.net');
WITH conn_id AS (
     INSERT INTO connection (server, username, nickname, realname) VALUES ('OFTC', 'jd', 'jd__', 'Julien') RETURNING id
) INSERT INTO channels (connection, name, password) SELECT id, '#foobar', 'mypassword' FROM conn_id;

SELECT is(name, '#foobar', 'Valid channel name') FROM channels;

PREPARE insert_invalid_channel_name AS INSERT INTO channels (connection, name) SELECT id, '%foobar' FROM connection;
SELECT throws_ok(
       'insert_invalid_channel_name',
       23514,
       'new row for relation "channels" violates check constraint "rfc2812"',
       'Invalid channel name with a %'
);

PREPARE insert_non_unique_channel AS INSERT INTO channels (connection, name) SELECT id, '#foobar' FROM connection;
SELECT throws_ok(
       'insert_non_unique_channel',
       23505,
       'duplicate key value violates unique constraint "channels_connection_name_key"',
       'Non-unique channel'
);

SELECT * FROM finish();
ROLLBACK;
