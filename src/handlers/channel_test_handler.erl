-module(channel_test_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_accepted/2,
         is_authorized/2,
         from_json/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {Name, Req2} = cowboy_req:binding(name, Req),
  State = #{name => Name},
  {ok, Req2, State}.

is_authorized(Req, State) ->
  req_utils:is_authorized(bearer, Req, State).

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, from_json}], Req, State}.

from_json(Req, State = #{user := User, email := Email, name := Name}) ->
  case db_channel:get(Email, Name) of
    {ok, Channel} ->
      Username = maps:get(name, User),
      Message = <<"This is a Holiday Ping test: ", Username/binary, " will be out on holidays.">>,
      remind_router:send(User, Channel, erlang:date(), Message),
      {true, Req, State};
    _ ->
      req_utils:error_response(404, <<"Channel not found.">>, Req)
  end.
