-module(test_utils).

-export([unique_email/0,
         api_request/3,
         api_request/4,
         api_request/5,
         create_user/0,
         create_user/1,
         create_user_with_token/0,
         create_user_with_token/1,
         delete_user/1,
         is_same_holiday/3,
         is_same_holiday/4]).

unique_email() ->
  erlang:list_to_binary("test_user" ++ ktn_random:string(5) ++ "@example.com").

api_request(Method, AccessToken, Path) ->
  api_request(Method, AccessToken, Path, <<"">>, []).

api_request(Method, AccessToken, Path, Data) ->
  api_request(Method, AccessToken, Path, Data, []).

api_request(Method, public, Path, Data, Options) ->
  api_request_internal(Method, [], Path, Data, Options);
api_request(Method, AccessToken, Path, Data, Options) ->
  Headers = [{<<"Authorization">>, <<"Bearer ", AccessToken/binary>>}],
  api_request_internal(Method, Headers, Path, Data, Options).

api_request_internal(Method, Headers, Path, Data, Options) ->
  %% TODO make url configurable
  Port = erlang:integer_to_list(hp_config:get(port)),
  Url = "http://localhost:" ++ Port ++ Path,
  Body = hp_json:encode(Data),
  AllHeaders = [{<<"Content-Type">>, <<"application/json">>} | Headers],
  case hackney:request(Method, Url, AllHeaders, Body, [with_body | Options]) of
    {ok, _, _, <<"">>} = Res -> Res;
    {ok, Status, ResHeaders, ResBody} -> {ok, Status, ResHeaders, hp_json:decode(ResBody)}
  end.

create_user() ->
  create_user(#{}).

create_user(Overrides) ->
  Email = unique_email(),
  Password = <<"S3cr3t!!">>,
  Body = maps:merge(#{
                       email => Email,
                       name => <<"John Doe">>,
                       password => Password,
                       country => <<"argentina">>
                     }, Overrides),

  {ok, 201, _, _} = api_request(post, public, "/api/users", Body),

  %% verify user
  Email2 = maps:get(email, Body),
  {ok, #{verification_code:= Code}} = db_user:get_verification(Email2),
  VerifyBody = #{code => Code, email => Email2},
  {ok, 204, _, _} =  api_request(post, public, "/api/users/confirmation/code", VerifyBody),

  Body.

create_user_with_token() ->
  create_user_with_token(#{}).

create_user_with_token(Overrides) ->
  #{email := Email, password := Password} = User = create_user(Overrides),
  {ok, 200, _, #{access_token := Token}} =
    test_utils:api_request(get, public, "/api/auth/token", <<"">>,
                           [{basic_auth, {Email, Password}}]),
  User#{token => Token}.

delete_user(Email) ->
  %% FIXME delete user via API, not db
  db_user:delete(Email).

is_same_holiday(Holiday, MM, DD, Name) ->
  Expected = hp_date:date_to_binary({hp_date:current_year(), MM, DD}),
  (maps:get(date, Holiday) == Expected) and (maps:get(name, Holiday) == Name).

is_same_holiday(Holiday, MM, DD) ->
  Expected = hp_date:date_to_binary({hp_date:current_year(), MM, DD}),
  maps:get(date, Holiday) == Expected.
