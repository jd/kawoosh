-- Start transaction and plan the tests.
BEGIN;
SELECT plan(4);

DELETE FROM users;

-- Valid users
INSERT INTO users (name, password) VALUES ('jd', 'lol');
SELECT is(name, 'jd', 'Valid username') FROM users WHERE name = 'jd';
SELECT ok(count(*) = 1, 'Valid username count') FROM users;

-- Empty password
PREPARE insert_no_password AS INSERT INTO users (name) VALUES ('#invalid');
SELECT throws_ok(
       'insert_no_password',
       23502,
       'null value in column "password" violates not-null constraint',
       'No password'
);

-- Invalid username check
PREPARE insert_invalid_user_name AS INSERT INTO users (name, password) VALUES ('#invalid', 'lol');
SELECT throws_ok(
       'insert_invalid_user_name',
       23514,
       'new row for relation "users" violates check constraint "users_name_check"',
       'Invalid user name with a #'
);

-- Finish the tests and clean up.
SELECT * FROM finish();
ROLLBACK;
