-module(test_utils).

-export([unique_email/0,
         api_request/3]).

unique_email() ->
    "test_user" ++ ktn_random:string(5) ++ "@example.com".

api_request(post, Path, Data) ->
    %% TODO make url configurable
    Port = erlang:integer_to_list(hp_config:get(port)),
    Url = "http://localhost:" ++ Port ++ Path,
    Body = hp_json:encode(Data),
    case hackney:post(Url, [{<<"Content-Type">>, <<"application/json">>}], Body, [with_body]) of
        {ok, _, _, <<"">>} = Res -> Res;
        {ok, Status, Headers, ResBody} -> {ok, Status, Headers, hp_json:decode(ResBody)}
    end.
