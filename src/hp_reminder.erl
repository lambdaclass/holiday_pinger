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
%% FIXME may be better to use a different process per each channel
%% TODO instead of tracking already sent by saving in the db, maybe add another server
%% that just knows how to send individual messages (and retries on failure)
%% that could later become a pool of workers, use a table instead of a mailbox, etc
handle_cast({send_reminders, User, HolidayDate}, State) ->
    io:format("Sending reminders for user ~p~n", [User]),
    %% TODO build a more meaningful message,
    Message = "dont forget!",

    Channels = hp_channel_db:get_user_channels(User),
    AlreadySent = fun (Channel) ->
                          not hp_reminder_db:is_already_sent(User, Channel, HolidayDate)
                  end,
    Pending = lists:filter(AlreadySent, Channels),
    SendFn = fun (Channel) ->
                     Handler = get_channel_handler(Channel),
                     Handler(User, HolidayDate, Message)
             end,
    lists:foreach(SendFn, Pending),
    {noreply, State};

handle_cast(Request, State) ->
    io:format("Unknown message ~p~n", [Request]),
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
get_channel_handler(Channel) ->
    fun (User, HolidayDate, Message) ->
            io:format("This is a holiday reminder: ~p~n", [Message]),
            hp_reminder_db:set_sent(User, Channel, HolidayDate)
    end.
