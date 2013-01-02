BEGIN;
SELECT plan(4);

DELETE FROM connection;
DELETE FROM servers;
DELETE FROM users;

INSERT INTO users (name) VALUES ('jd');
INSERT INTO servers (name, address) VALUES ('OFTC', '  IRC.oftc.net');

INSERT INTO connection (server, username, nickname) VALUES ('OFTC', 'jd', 'jd');
SELECT is(username, 'jd', 'Valid nickname') FROM connection;
SELECT ok(count(*) = 1, 'Valid server count') FROM connection;

PREPARE insert_invalid_nickname AS INSERT INTO connection (server, username, nickname) VALUES ('OFTC', 'jd', '#jdd');
SELECT throws_ok(
       'insert_invalid_nickname',
       23514,
       'new row for relation "connection" violates check constraint "connection_nickname_check"',
       'Invalid nickname'
);

PREPARE insert_invalid_username AS INSERT INTO connection (server, username, nickname) VALUES ('OFTC', 'foobar.net', 'foobar');
SELECT throws_ok(
       'insert_invalid_username',
       23503,
       'insert or update on table "connection" violates foreign key constraint "connection_username_fkey"',
       'Non-existent user name'
);

SELECT * FROM finish();
ROLLBACK;
