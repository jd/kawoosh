BEGIN;
SELECT plan(1);

DELETE FROM servers;
DELETE FROM users;

INSERT INTO users (name) VALUES ('jd');
WITH server_id AS (
     INSERT INTO servers (name, username, address, nickname, realname) VALUES ('OFTC', 'jd', 'irc.oftc.net', 'jd__', 'Julien') RETURNING id
)
INSERT INTO logs (server, source, command, target, payload) SELECT id, 'myserver', 'PRIVMSG', '#foobar', 'hello world' FROM server_id;

SELECT is(source, 'myserver', 'Valid source name') FROM logs;

SELECT * FROM finish();
ROLLBACK;
