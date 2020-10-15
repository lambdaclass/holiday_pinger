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
  Body = request_body(Data),
  Headers2 = [{<<"Content-Type">>, <<"application/json">>},
              {<<"Accept">>, <<"application/json">>} | Headers],
  {ok, Status, ResHeaders, ResBody} =
    hackney:Method(Url, Headers2, Body, [with_body]),
  lager:debug("Response ~p ~p ~p", [Status, ResBody, ResHeaders]),
  Decoded = decode_response(proplists:get_value(<<"Content-Type">>, ResHeaders), ResBody),
  {ok, Status, ResHeaders, Decoded}.

decode_response(<<"application/json", _/binary>>, ResBody) ->
  hp_json:decode(ResBody);
decode_response(_, ResBody) ->
  ResBody.

%% Passing Data=null to hp_json:encode means the body becomes null. This makes the google
%% api break and return a 400 status, hence GET requests pass <<>> in the Body.
request_body(Data) ->
  case Data of
    null ->
      <<>>;
    _ ->
      hp_json:encode(Data)
  end.
