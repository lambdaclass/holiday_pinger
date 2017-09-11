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
  {ok, Req, #{}}.

is_authorized(Req, State) ->
  req_utils:is_authorized(bearer, Req, State).

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"PUT">>],
   Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State = #{email := Email}) ->
  {ok, Holidays} = db_reminder:get_reminder_config(Email),
  Body = hp_json:encode(Holidays),
  {Body, Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, from_json}], Req, State}.

from_json(Req, State = #{email := Email}) ->
  %% only configuration can be updated for now
  {ok, Body, Req2} = cowboy_req:body(Req),

  #{same_day := SameDay, days_before := DaysBefore} = hp_json:decode(Body),
  ok = db_reminder:update_reminder_config(Email, SameDay, DaysBefore),

  Req3 = cowboy_req:set_resp_body(Body, Req2),
  {true, Req3, State}.
