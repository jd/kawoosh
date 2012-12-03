-- Start transaction and plan the tests.
BEGIN;
SELECT plan(5);

DELETE FROM networks;
DELETE FROM servers;

-- Default data
INSERT INTO networks (name) VALUES ('OFTC');
INSERT INTO networks (name) VALUES ('Freenode');

-- Tests
INSERT INTO servers (address, network) VALUES ('  IRC.oftc.net', 'OFTC');
SELECT is(address, 'irc.oftc.net', 'Valid server address') FROM servers;
SELECT ok(count(*) = 1, 'Valid server count') FROM servers;

PREPARE insert_invalid_server_address AS INSERT INTO servers (address, network) VALUES ('invalid%.irc.net', 'OFTC');
SELECT throws_ok(
       'insert_invalid_server_address',
       23514,
       'new row for relation "servers" violates check constraint "servers_address_check"',
       'Invalid server address with a %'
);

PREPARE insert_non_unique_server_address AS INSERT INTO servers (address, network) VALUES ('irc.OFTC.net', 'Freenode');
SELECT throws_ok(
       'insert_non_unique_server_address',
       23505,
       'duplicate key value violates unique constraint "servers_pkey"',
       'Non-unique server address'
);

PREPARE insert_non_existent_server_network AS INSERT INTO servers (address, network) VALUES ('irc.dtc.net', 'DC');
SELECT throws_ok(
       'insert_non_existent_server_network',
       23503,
       'insert or update on table "servers" violates foreign key constraint "servers_network_fkey"',
       'Non-existent network'
);

-- Finish the tests and clean up.
SELECT * FROM finish();
ROLLBACK;
