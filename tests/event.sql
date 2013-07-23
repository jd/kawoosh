BEGIN;
SELECT plan(1);

DELETE FROM connection;
DELETE FROM servers;
DELETE FROM users;

INSERT INTO users (name) VALUES ('jd');
INSERT INTO servers (name, address) VALUES ('OFTC', 'irc.oftc.net');
WITH server_id AS (
     INSERT INTO connection (server, username, nickname, realname) VALUES ('OFTC', 'jd', 'jd__', 'Julien') RETURNING id
)
INSERT INTO event (connection, source, command, target, payload) SELECT id, 'myserver', 'PRIVMSG', '#foobar', 'hello world' FROM server_id;

SELECT is(source, 'myserver', 'Valid source name') FROM event;

SELECT * FROM finish();
ROLLBACK;
