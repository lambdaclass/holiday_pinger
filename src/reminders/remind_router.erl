-module(remind_router).
-behaviour(gen_server).

%%% This module receives user, channel and holiday data, builds a reminder
%%% and sends it through the channel

-export([start_link/0,

         send/3,
         send/4,

         init/1,
         handle_call/3,
         handle_cast/2]).

send(User, Channel, HolidayDate) ->
  {ok, Pid} = supervisor:start_child(remind_router_sup, []),
  gen_server:cast(Pid, {send_reminder, User, Channel, HolidayDate}).

send(User, Channel, HolidayDate, Message) ->
  {ok, Pid} = supervisor:start_child(remind_router_sup, []),
  gen_server:cast(Pid, {send_message, User, Channel, HolidayDate, Message}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, []}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({send_reminder, User, Channel, HolidayDate}, State) ->
  Message = build_message(User, HolidayDate),
  handle_cast({send_message, User, Channel, HolidayDate, Message}, State);

handle_cast({send_message, User, Channel, HolidayDate, Message}, State) ->
  #{name := ChannelName, configuration := Config, type := Type} = Channel,
  lager:debug("Sending reminders for user ~p and channel ~p",
              [maps:get(name, User), ChannelName]),

  Handler = get_handler(Type),
  Handler:handle(User, HolidayDate, Config, Message),

  {noreply, State};

handle_cast(Request, State) ->
  lager:warning("Unknown message: ~p", [Request]),
  {noreply, State}.

%%% internal
%% at some point the reminder message will be part of the channel
build_message(#{name := UserName}, {Y, M, D}) ->
  list_to_binary(
    io_lib:format(<<"This is a holiday reminder: ~s will be out on ~2..0B/~2..0B/~B.">>,
                  [UserName, D, M, Y])).

get_handler(slack) -> slack_channel;
get_handler(console) -> console_channel;
get_handler(ets) -> ets_channel;
get_handler(webhook) -> webhook_channel;
get_handler(email) -> email_channel.
