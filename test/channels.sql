BEGIN;
SELECT plan(3);

DELETE FROM users;

INSERT INTO users (name) VALUES ('jd');
WITH server_id AS (
     INSERT INTO servers (name, address, username, nickname, realname) VALUES ('OFTC', 'irc.oftc.net', 'jd', 'jd__', 'Julien') RETURNING id
) INSERT INTO channels (server, name, password) SELECT id, '#foobar', 'mypassword' FROM server_id;

SELECT is(name, '#foobar', 'Valid channel name') FROM channels;

PREPARE insert_invalid_channel_name AS INSERT INTO channels (server, name) SELECT id, '%foobar' FROM servers;
SELECT throws_ok(
       'insert_invalid_channel_name',
       23514,
       'new row for relation "channels" violates check constraint "rfc2812"',
       'Invalid channel name with a %'
);

PREPARE insert_non_unique_channel AS INSERT INTO channels (server, name) SELECT id, '#foobar' FROM servers;
SELECT throws_ok(
       'insert_non_unique_channel',
       23505,
       'duplicate key value violates unique constraint "channels_server_name_key"',
       'Non-unique channel'
);

SELECT * FROM finish();
ROLLBACK;
