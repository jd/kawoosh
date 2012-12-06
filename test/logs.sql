BEGIN;
SELECT plan(1);

DELETE FROM network_connections;
DELETE FROM networks;
DELETE FROM users;

INSERT INTO users (name) VALUES ('jd');
INSERT INTO networks (name) VALUES ('OFTC');
WITH network_connection_id AS (
     INSERT INTO network_connections (username, network, nickname, realname) VALUES ('jd', 'OFTC', 'jd__', 'Julien') RETURNING id
)
INSERT INTO logs (network_connection, source, command, target, payload) SELECT id, 'myserver', 'PRIVMSG', '#foobar', 'hello world' FROM network_connection_id;

SELECT is(source, 'myserver', 'Valid source name') FROM logs;

SELECT * FROM finish();
ROLLBACK;
