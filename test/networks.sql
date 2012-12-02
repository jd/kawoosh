-- Start transaction and plan the tests.
BEGIN;
SELECT plan(3);

DELETE FROM networks;

-- Valid users
INSERT INTO networks (name) VALUES ('OFTC');
SELECT is(name, 'OFTC', 'Valid network name') FROM networks;
SELECT ok(count(*) = 1, 'Valid network count') FROM networks;

-- Invalid network check
PREPARE insert_invalid_network_name AS INSERT INTO networks (name) VALUES ('invalid%');
SELECT throws_ok(
       'insert_invalid_network_name',
       23514,
       'new row for relation "networks" violates check constraint "networks_name_check"',
       'Invalid network name with a %'
);

-- Finish the tests and clean up.
SELECT * FROM finish();
ROLLBACK;
