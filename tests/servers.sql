BEGIN;
SELECT plan(5);

DELETE FROM connection;
DELETE FROM servers;
DELETE FROM users;

INSERT INTO users (name, password) VALUES ('jd', 'lol');
INSERT INTO servers (name, address) VALUES ('OFTC', '  IRC.oftc.net');
SELECT is(address, 'irc.oftc.net', 'Valid server address') FROM servers;
SELECT ok(count(*) = 1, 'Valid server count') FROM servers;

PREPARE insert_invalid_server_name AS INSERT INTO servers (name, address) VALUES ('S€rver!', 'irc.net');
SELECT throws_ok(
       'insert_invalid_server_name',
       23514,
       'new row for relation "servers" violates check constraint "servers_name_check"',
       'Invalid server name with UTF-8 char'
);

PREPARE insert_invalid_server_address AS INSERT INTO servers (name, address) VALUES ('IRC', 'invalid%.irc.net');
SELECT throws_ok(
       'insert_invalid_server_address',
       23514,
       'new row for relation "servers" violates check constraint "servers_address_check"',
       'Invalid server address with a %'
);

PREPARE insert_invalid_server_port AS INSERT INTO servers (name, address, port) VALUES ('IRC', 'invalid%.irc.net', 123234);
SELECT throws_ok(
       'insert_invalid_server_port',
       23514,
       'new row for relation "servers" violates check constraint "servers_port_check"',
       'Invalid server port > 65535'
);

SELECT * FROM finish();
ROLLBACK;
