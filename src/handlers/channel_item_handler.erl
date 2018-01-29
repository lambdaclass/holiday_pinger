-module(channel_item_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         content_types_accepted/2,
         is_authorized/2,
         to_json/2,
         from_json/2,
         delete_resource/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {Name, Req2} = cowboy_req:binding(name, Req),
  State = #{name => Name},
  {ok, Req2, State}.

is_authorized(Req, State) ->
  req_utils:is_authorized(bearer, Req, State).

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"PUT">>, <<"DELETE">>],
   Req, State}.

resource_exists(Req, State = #{email := Email, name := Name}) ->
  case db_channel:get(Email, Name) of
    {ok, Channel} -> {true, Req, State#{channel => Channel, is_new => false}};
    _ -> {false, Req, State#{is_new => true}}
  end.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State = #{channel := Channel}) ->
  Body = hp_json:encode(Channel),
  {Body, Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, from_json}], Req, State}.

%% PUT for create
from_json(Req, State = #{is_new := true,
                         email := Email,
                         name := Name}) ->
  {ok, Body, Req2} = cowboy_req:body(Req),

  case hp_json:decode(Body) of
    #{
       type := Type,
       configuration := Config,
       reminder_days_before := DaysBefore,
       reminder_time := Time,
       reminder_timezone := TimeZone
     } when is_list(DaysBefore), length(DaysBefore) =< 5 ->
      CleanConfig = maps:filter(fun not_empty_value/2, Config),
      {ok, _} = db_channel:create(Email, Name, Type, CleanConfig, DaysBefore, Time, TimeZone),
      {ok, Channel} = db_channel:get(Email, Name),
      send_creation_notification(Channel, State),
      Req3 = cowboy_req:set_resp_body(Body, Req2),
      {true, Req3, State};
    _ ->
      %% TODO more sophisticated validations and error messages
      req_utils:error_response(400, <<"Missing or invalid field">>, Req2)
  end;

%% PUT for update
from_json(Req, State = #{is_new := false,
                         email := Email,
                         name := Name,
                         channel := Channel}) ->

  {ok, Body, Req2} = cowboy_req:body(Req),

  case hp_json:decode(Body) of
    #{
       configuration := Config,
       reminder_days_before := DaysBefore,
       reminder_time := Time,
       reminder_timezone := TimeZone
     } when is_list(DaysBefore), length(DaysBefore) =< 5 ->
      CleanConfig = maps:filter(fun not_empty_value/2, Config),
      ok = db_channel:update(Email, Name, CleanConfig, DaysBefore, Time, TimeZone),
      reminder:regenerate(Email, Name),

      RespBody = Channel#{
                   configuration := Config,
                   reminder_days_before := DaysBefore,
                   reminder_time := Time
                  },
      Encoded = hp_json:encode(RespBody),
      Req3 = cowboy_req:set_resp_body(Encoded, Req2),
      {true, Req3, State};
    _ ->
      %% TODO more sophisticated validations and error messages
      req_utils:error_response(400, <<"Missing or invalid field">>, Req2)
  end.

delete_resource(Req, State = #{email := Email, name := Name}) ->
  {ok, Channel} = db_channel:get(Email, Name),
  send_delete_notification(Channel, State),
  ok = db_channel:delete(Email, Name),
  {true, Req, State}.

%%% internal
not_empty_value(_K, null) ->
  false;
not_empty_value(_K, Value) when is_binary(Value) ->
  re:replace(Value, "\\s+", "", [global,{return,binary}]) /= <<"">>;
not_empty_value(_K, _Value) ->
  true.

send_delete_notification(Channel=#{name := Name}, #{user := User}) ->
  Msg = <<"Removed a remainder '", Name/binary, "'.">>,
  remind_router:send_message(User, Channel, Msg).

send_creation_notification(Channel=#{name := Name}, #{user := User}) ->
  Msg = <<"Crated a remainder '", Name/binary, "'.">>,
  remind_router:send_message(User, Channel, Msg).
