-- Start transaction and plan the tests.
BEGIN;
SELECT plan(6);

DELETE FROM servers;
DELETE FROM users;

INSERT INTO users (name) VALUES ('jd');
INSERT INTO servers (name, address, username, nickname) VALUES ('OFTC', '  IRC.oftc.net', 'jd', 'jd__');
SELECT is(address, 'irc.oftc.net', 'Valid server address') FROM servers;
SELECT ok(count(*) = 1, 'Valid server count') FROM servers;

PREPARE insert_invalid_server_address AS INSERT INTO servers (name, address, username, nickname) VALUES ('IRC', 'invalid%.irc.net', 'jd__', 'jd');
SELECT throws_ok(
       'insert_invalid_server_address',
       23514,
       'new row for relation "servers" violates check constraint "servers_address_check"',
       'Invalid server address with a %'
);

PREPARE insert_invalid_server_port AS INSERT INTO servers (name, address, port, username, nickname) VALUES ('IRC', 'invalid%.irc.net', 123234, 'jd', 'jd');
SELECT throws_ok(
       'insert_invalid_server_port',
       23514,
       'new row for relation "servers" violates check constraint "servers_port_check"',
       'Invalid server port > 65535'
);

PREPARE insert_invalid_username AS INSERT INTO servers (name, address, username, nickname) VALUES ('foobar', 'foobar.net', 'foobar', 'foobar');
SELECT throws_ok(
       'insert_invalid_username',
       23503,
       'insert or update on table "servers" violates foreign key constraint "servers_username_fkey"',
       'Non-existent user name'
);

PREPARE insert_invalid_nickname AS INSERT INTO servers (name, address, username, nickname) VALUES ('OFTC', 'irc', 'jd', '#jdd');
SELECT throws_ok(
       'insert_invalid_nickname',
       23514,
       'new row for relation "servers" violates check constraint "servers_nickname_check"',
       'Invalid nickname'
);

-- Finish the tests and clean up.
SELECT * FROM finish();
ROLLBACK;
