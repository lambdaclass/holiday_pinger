-module(remind_router).
-behaviour(gen_server).

%%% This module receives user, channel and holiday data, builds a reminder
%%% and sends it through the channel

-export([start_link/0,

         send/3,
         send_test/3,
         send_message/3,

         init/1,
         handle_call/3,
         handle_cast/2]).

send(User, Channel, HolidayDate) ->
  {ok, Pid} = supervisor:start_child(remind_router_sup, []),
  Message = build_message(User, HolidayDate),
  gen_server:cast(Pid, {send_reminder, User, Channel, HolidayDate, Message, false}).

send_test(User, Channel, HolidayDate) ->
  Username = maps:get(name, User),
  Message = <<"This is a Holiday Ping test: ", Username/binary, " will be out on holidays.">>,
  {ok, Pid} = supervisor:start_child(remind_router_sup, []),
  gen_server:cast(Pid, {send_reminder, User, Channel, HolidayDate, Message, true}).

send_message(User, Channel, Message) ->
  UserName = maps:get(name, User),
  FinalMessage = <<UserName/binary, ": ", Message/binary>>,
  {ok, Pid} = supervisor:start_child(remind_router_sup, []),
  gen_server:cast(Pid, {send_reminder, User, Channel, erlang:date(), FinalMessage, true}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, []}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({send_reminder, User, Channel, HolidayDate, Message, IsTest}, State) ->
  #{name := ChannelName, configuration := Config, type := Type} = Channel,
  UserName = maps:get(name, User),
  Email = maps:get(email, User),
  lager:debug("Sending reminders for user ~p and channel ~p",
              [UserName, ChannelName]),

  case check_should_send(Email, Channel) of
    ok ->
      Handler = get_handler(Type),
      case Handler:handle(User, HolidayDate, Config, Message) of
        {ok, SentReminders} ->
          db_reminder:delete(Email, ChannelName, HolidayDate, erlang:date()),
          reminder:log_reminders(Email, ChannelName, SentReminders, IsTest);
        Error ->
          lager:warning(<<"Error sending reminders ~p">>, [Error])
      end;
    already_sent ->
      db_reminder:delete(Email, ChannelName, HolidayDate, erlang:date());
    limit_exceeded ->
      lager:warning(<<"User ~p exceeded channel limit for type ~p">>,
                    [UserName, Type])
  end,

  {noreply, State};

handle_cast(Request, State) ->
  lager:warning("Unknown message: ~p", [Request]),
  {noreply, State}.

%%% internal
build_message(#{name := UserName}, Date) ->
  HumanDate = hp_date:human_date(Date),
  <<"This is a holiday reminder: ",
    UserName/binary, " will be out ", HumanDate/binary, ".">>.

get_handler(slack) -> slack_channel;
get_handler(console) -> console_channel;
get_handler(ets) -> ets_channel;
get_handler(webhook) -> webhook_channel;
get_handler(email) -> email_channel.

check_should_send(Email, #{name := ChannelName, type := ChannelType}) ->
  case reminder:is_already_sent(Email, ChannelName) of
    true -> already_sent;
    false -> reminder:check_limit(Email, ChannelType)
  end.
