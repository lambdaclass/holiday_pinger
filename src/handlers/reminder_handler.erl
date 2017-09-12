-module(reminder_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         is_authorized/2,
         to_json/2,
         from_json/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {Channel, Req2} = cowboy_req:binding(channel, Req),
  State = #{channel => Channel},
  {ok, Req2, State}.

is_authorized(Req, State) ->
  req_utils:is_authorized(bearer, Req, State).

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"PUT">>],
   Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State = #{email := Email, channel := Channel}) ->
  case db_reminder:get_reminder_config(Email, Channel) of
    {ok, Config} ->
      Body = hp_json:encode(Config),
      {Body, Req, State};
    {error, channel_not_found} ->
      req_utils:error_response(404, <<"Channel not found">>, Req)
  end.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, from_json}], Req, State}.

from_json(Req, State = #{email := Email, channel := Channel}) ->
  %% only configuration can be updated for now
  {ok, Body, Req2} = cowboy_req:body(Req),

  #{same_day := SameDay, days_before := DaysBefore} = hp_json:decode(Body),
  case db_reminder:update_reminder_config(Email, Channel, SameDay, DaysBefore) of
    ok -> Req3 = cowboy_req:set_resp_body(Body, Req2),
          {true, Req3, State};
    {error, channel_not_found} ->
      req_utils:error_response(404, <<"Channel not found">>, Req)
  end.
