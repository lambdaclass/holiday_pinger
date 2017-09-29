-module(db_user).

-export([create_holiday_user/4,
         create_github_user/2,
         get/1,
         delete/1,
         get_with_password/1,
         get_verification/1,
         set_verified/1,
         user_keys/0]).

%% needed so atoms exist.
user_keys () -> [email, password, name].

create_holiday_user(Email, Name, Password, VerificationCode) ->
  Q = <<"INSERT INTO users(email, name, password, auth_type, verification_code, verification_sent_at) "
        "VALUES($1, $2, $3, 'holiday', $4, (now() at time zone 'utc')) "
        "RETURNING email, name ">>,
  case db:query(Q, [Email, Name, Password, VerificationCode]) of
    {ok, [Result | []]} -> {ok, Result};
    {error, unique_violation} -> {error, user_already_exists}
  end.

create_github_user(Email, Name) ->
  Q = <<"INSERT INTO users(email, name, auth_type, verified)"
        "VALUES($1, $2, 'github', true) RETURNING email, name ">>,
  case db:query(Q, [Email, Name]) of
    {ok, [Result | []]} -> {ok, Result};
    {error, unique_violation} -> {error, user_already_exists}
  end.

get(Email) ->
  Q = <<"SELECT email, name FROM users WHERE email = $1">>,
  case db:query(Q, [Email]) of
    {ok, []} -> {error, not_found};
    {ok, [User | []]} -> {ok, User}
  end.

get_with_password(Email) ->
  Q = <<"SELECT email, name, verified, password FROM users "
        "WHERE email = $1 AND auth_type = 'holiday'">>,
  case db:query(Q, [Email]) of
    {ok, []} -> {error, not_found};
    {ok, [User | []]} -> {ok, User}
  end.

get_verification(Email) ->
  Q = <<"SELECT verified, verification_code, "
        "EXTRACT (EPOCH FROM (now() at time zone 'utc')) - EXTRACT (EPOCH FROM verification_sent_at) as sent_seconds_ago "
        "FROM users WHERE email = $1 AND auth_type = 'holiday'">>,
  case db:query(Q, [Email]) of
    {ok, []} -> {error, not_found};
    {ok, [User | []]} -> {ok, User}
  end.

set_verified(Email) ->
  Q = <<"UPDATE \"users\" SET verified = true WHERE email = $1">>,
  db:query(Q, [Email]).

delete(Email) ->
  Q = <<"DELETE FROM users WHERE email = $1">>,
  db:query(Q, [Email]).
