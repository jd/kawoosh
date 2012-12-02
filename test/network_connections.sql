-- Start transaction and plan the tests.
BEGIN;
SELECT plan(6);

DELETE FROM network_connections;
DELETE FROM users;
DELETE FROM networks;

INSERT INTO networks (name) VALUES ('OFTC');
INSERT INTO users (name) VALUES ('jd');

-- Valid data
INSERT INTO network_connections (username, network, nickname, realname) VALUES ('jd', 'OFTC', 'jd__', 'Julien');
SELECT is(network, 'OFTC', 'Valid network connection') FROM network_connections;
SELECT ok(count(*) = 1, 'Valid network connection count') FROM network_connections;

-- Tests with invalid data
PREPARE insert_invalid_username AS INSERT INTO network_connections (username, network, nickname, realname) VALUES ('foobar', 'OFTC', 'jd__', 'Julien');
SELECT throws_ok(
       'insert_invalid_username',
       23503,
       'insert or update on table "network_connections" violates foreign key constraint "network_connections_username_fkey"',
       'Non-existent user name'
);

PREPARE insert_invalid_network AS INSERT INTO network_connections (username, network, nickname, realname) VALUES ('jd', 'Freenode', 'jd__', 'Julien');
SELECT throws_ok(
       'insert_invalid_network',
       23503,
       'insert or update on table "network_connections" violates foreign key constraint "network_connections_network_fkey"',
       'Non-existent network name'
);

PREPARE insert_invalid_nickname AS INSERT INTO network_connections (username, network, nickname, realname) VALUES ('jd', 'OFTC', '#jd__', 'Julien');
SELECT throws_ok(
       'insert_invalid_nickname',
       23514,
       'new row for relation "network_connections" violates check constraint "network_connections_nickname_check"',
       'Invalid nickname'
);

-- Uniqness on network and username
PREPARE insert_non_unique_network_user AS INSERT INTO network_connections (username, network, nickname, realname) VALUES ('jd', 'OFTC', 'jd___', 'Julien Bis');
SELECT throws_ok(
       'insert_non_unique_network_user',
       23505,
       'duplicate key value violates unique constraint "network_connections_network_username_key"',
       'Non uniq user and network'
);

-- Finish the tests and clean up.
SELECT * FROM finish();
ROLLBACK;
