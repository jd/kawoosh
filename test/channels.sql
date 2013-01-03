BEGIN;
SELECT plan(6);

DELETE FROM users;

INSERT INTO users (name) VALUES ('jd');
INSERT INTO servers (name, address) VALUES ('OFTC', 'irc.oftc.net');
INSERT INTO servers (name, address) VALUES ('Freenode', 'irc.freenode.net');
WITH conn_id AS (
     INSERT INTO connection (server, username, nickname, realname) VALUES ('OFTC', 'jd', 'jd__', 'Julien') RETURNING id
) INSERT INTO channels (connection, name, password) SELECT id, '#foobar', 'mypassword' FROM conn_id;
INSERT INTO connection (server, username, nickname) VALUES ('Freenode', 'jd', 'jd_');

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

PREPARE update_channel_name AS UPDATE channels SET name='#blabla' WHERE name='#foobar';
SELECT throws_ok(
       'update_channel_name',
       'P0001',
       'You are not allowed to modify name or connection of a channel',
       'Update channel name'
);

PREPARE update_channel_connection AS UPDATE channels SET connection=(SELECT id FROM connection where server='Freenode');
SELECT throws_ok(
       'update_channel_connection',
       'P0001',
       'You are not allowed to modify name or connection of a channel',
       'Update channel name'
);

UPDATE channels SET password='p4ss' WHERE name='#foobar';
SELECT is(password, 'p4ss', 'Update channel password') FROM channels WHERE name='#foobar';

SELECT * FROM finish();
ROLLBACK;
