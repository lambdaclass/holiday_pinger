-module(users_login_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [login_with_valid_user,
     fail_login_on_missing_field,
     fail_login_on_unknown_user,
     fail_login_on_wrong_password].

login_with_valid_user() ->
    %% make sure given access token works for accessing the API
    ok.

fail_login_on_missing_fields() ->
    ok.

fail_login_on_unknown_user() ->
    ok.

fail_login_on_wrong_password() ->
    ok.
