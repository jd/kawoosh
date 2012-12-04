BEGIN;
SELECT plan(3);

INSERT INTO users (name) VALUES ('jd');
INSERT INTO networks (name) VALUES ('OFTC');
WITH network_connection_id AS (
     INSERT INTO network_connections (username, network, nickname, realname) VALUES ('jd', 'OFTC', 'jd__', 'Julien') RETURNING id
) INSERT INTO channels (network_connection, name, password) SELECT id, '#foobar', 'mypassword' FROM network_connection_id;

SELECT is(name, '#foobar', 'Valid channel name') FROM channels;

PREPARE insert_invalid_channel_name AS INSERT INTO channels (network_connection, name) SELECT id, '%foobar' from network_connections;
SELECT throws_ok(
       'insert_invalid_channel_name',
       23514,
       'new row for relation "channels" violates check constraint "rfc2812"',
       'Invalid channel name with a %'
);

PREPARE insert_non_unique_channel AS INSERT INTO channels (network_connection, name) SELECT id, '#foobar' from network_connections;
SELECT throws_ok(
       'insert_non_unique_channel',
       23505,
       'duplicate key value violates unique constraint "channels_network_connection_name_key"',
       'Non-unique channel'
);

SELECT * FROM finish();
ROLLBACK;
