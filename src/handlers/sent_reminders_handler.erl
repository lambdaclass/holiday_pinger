-module(sent_reminders_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         to_json/2
        ]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {Channel, Req2} = cowboy_req:binding(channel, Req),
  State = #{channel => Channel},
  {ok, Req2, State}.

is_authorized(Req, State) ->
  req_utils:is_authorized(bearer, Req, State).

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>],
   Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State = #{email := Email, channel := Channel}) ->
  case db_reminder:get_recent(Email, Channel) of
    {ok, Reminders} ->
      Body = hp_json:encode(Reminders),
      {Body, Req, State};
    {error, channel_not_found} ->
      req_utils:error_response(404, <<"Channel not found">>, Req)
  end.
