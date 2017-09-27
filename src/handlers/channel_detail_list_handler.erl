-module(channel_detail_list_handler).

-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         to_json/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

is_authorized(Req, _State) ->
  req_utils:is_authorized(bearer, Req, #{}).

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State = #{email := Email}) ->
  {ok, Channels} = db_channel:list(Email),
  Body = lists:map(fun(Channel) ->
                       get_details(Email, Channel)
                   end, Channels),

  {hp_json:encode(Body), Req, State}.

%%% internal
get_details(Email, #{name := ChannelName} = Channel) ->
  {ok, Holidays} = db_holiday:get_channel_holidays(Email, ChannelName),
  {ok, Reminders} = db_reminder:get_recent(Email, ChannelName),
  Channel#{holidays => Holidays, reminders => Reminders}.
