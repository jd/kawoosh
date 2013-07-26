BEGIN;
SELECT plan(4);

DELETE FROM users;

INSERT INTO users (name, password) VALUES ('jd', 'lol');
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
       'duplicate key value violates unique constraint "channels_pkey"',
       'Non-unique channel'
);

UPDATE channels SET password='p4ss' WHERE name='#foobar';
SELECT is(password, 'p4ss', 'Update channel password') FROM channels WHERE name='#foobar';

SELECT * FROM finish();
ROLLBACK;
