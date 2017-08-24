-module(hp_reminder).
-behaviour(gen_server).

-export([start_link/0,

         send/2,
         send_to_channel/2,

         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(HANDLERS, #{
          <<"slack">> => fun slack_channel:handle/2,
          <<"console">> => fun console_channel:handle/2,
          <<"ets">> => fun ets_channel:handle/2
         }).

send(User, HolidayDate) ->
    {ok, Pid} = supervisor:start_child(hp_reminder_sup, []),
    gen_server:cast(Pid, {send_reminders, User, HolidayDate}).

%% Exporting this function so we can test individual channels
%% In the future we'll start the child of another simple_one_for_one that
%% handles the actual reminder delivery on each channel
send_to_channel((#{type := Type, configuration := Config}), Message) ->
    Handler = maps:get(Type, ?HANDLERS),
    Handler(Config, Message).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

%% Send the message to each channel the user has configured
handle_cast({send_reminders, User, HolidayDate}, State) ->
    lager:debug("Sending reminders for user ~p", [User]),

    Message = build_message(User, HolidayDate),
    Email = maps:get(email, User),
    {ok, Channels} = db_channel:list(Email),
    lists:foreach(fun(Channel) ->
                          send_to_channel(Channel, Message)
                  end, Channels),
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
