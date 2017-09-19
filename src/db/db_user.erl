-module(db_user).

-export([create_holiday_user/3,
         create_github_user/2,
         get/1,
         delete/1,
         get_with_password/1,
         user_keys/0]).

%% needed so atoms exist.
user_keys () -> [email, password, name].

create_holiday_user(Email, Name, Password) ->
  Q = <<"INSERT INTO users(email, name, password, auth_type)"
        "VALUES($1, $2, $3, 'holiday') RETURNING email, name ">>,
  case db:query(Q, [Email, Name, Password]) of
    {ok, [Result | []]} -> {ok, Result};
    {error, unique_violation} -> {error, user_already_exists}
  end.

create_github_user(Email, Name) ->
  Q = <<"INSERT INTO users(email, name, auth_type)"
        "VALUES($1, $2, 'github') RETURNING email, name ">>,
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
  Q = <<"SELECT email, name, password FROM users "
        "WHERE email = $1 AND auth_type = 'holiday'">>,
  case db:query(Q, [Email]) of
    {ok, []} -> {error, not_found};
    {ok, [User | []]} -> {ok, User}
  end.

delete(Email) ->
  Q = <<"DELETE FROM users WHERE email = $1">>,
  db:query(Q, [Email]).
