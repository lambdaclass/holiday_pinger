-module(hp_reminder).
-behaviour(gen_server).

-export([start_link/0,

         send/2,

         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(HANDLERS, #{
          slack => fun slack_channel:handle/2,
          console => fun console_channel:handle/2
         }).

send(User, HolidayDate) ->
    {ok, Pid} = supervisor:start_child(hp_reminder_sup, []),
    gen_server:cast(Pid, {send_reminders, User, HolidayDate}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

%% Send the message to each channel the user has configured, dont send the same reminder twice
%% TODO add another simple_one_for_one supervisor whose children send the actual message to each channel
handle_cast({send_reminders, User, HolidayDate}, State) ->
    lager:debug("Sending reminders for user ~p", [User]),

    Message = build_message(User, HolidayDate),
    Channels = db_channel:get_user_channels(User),
    SendFn = fun (Channel = #{type := Type}) ->
                     Handler = maps:get(Type, ?HANDLERS),
                     Handler(Channel, Message)
             end,
    lists:foreach(SendFn, Channels),
    {noreply, State};

handle_cast(Request, State) ->
    lager:warning("Unknown message: ~p", [Request]),
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% internal
build_message(#{name := UserName}, {Y, M, D}) ->
    list_to_binary(
      io_lib:format(<<"This is a holiday reminder: ~s will be out on ~2..0B/~2..0B/~B.">>,
                    [UserName, D, M, Y])).
