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
    %% TODO build a more meaningful message,
    Message = "dont forget!",

    Channels = db_channel:get_user_channels(User),
    SendFn = fun (Channel) ->
                     Handler = get_channel_handler(Channel),
                     Handler(User, HolidayDate, Message)
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

%%%% internal

%% Take the channel config and return a function that sends reminder
%% through that channel
%% TODO move each handler to a specific file
get_channel_handler(#{type := slack}) ->
    fun (#{<<"name">> := User}, _HolidayDate, Message) ->
            lager:info("This is a SLACK holiday reminder from ~s: ~s", [User, Message])
    end;
get_channel_handler((#{type := mail})) ->
    fun (#{<<"name">> := User}, _HolidayDate, Message) ->
            lager:info("This is a MAIL holiday reminder from ~s: ~s", [User, Message])
    end.
