-module(hp_request).

-export([post_json/2,
         post_json/3,
         get_json/1,
         get_json/2]).

post_json(Url, Data) ->
  post_json(Url, Data, []).

post_json(Url, Data, Headers) ->
  request_json(post, Url, Data, Headers).

get_json(Url) ->
  get_json(Url, []).

get_json(Url, Headers) ->
  request_json(get, Url, null, Headers).

%% internal
request_json(Method, Url, Data, Headers) ->
  Body = hp_json:encode(Data),
  Headers2 = [{<<"Content-Type">>, <<"application/json">>},
              {<<"Accept">>, <<"application/json">>} | Headers],
  {ok, Status, ResHeaders, ResBody} =
    hackney:Method(Url, Headers2, Body, [with_body]),
  {ok, Status, ResHeaders, hp_json:decode(ResBody)}.
