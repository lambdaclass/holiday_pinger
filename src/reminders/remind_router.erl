-module(remind_router).
-behaviour(gen_server).

%%% This module receives user and holiday data, builds a reminder message and
%%% sends it through each channel the user has configured

-export([start_link/0,

         send/2,

         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

send(User, HolidayDate) ->
    {ok, Pid} = supervisor:start_child(remind_router_sup, []),
    gen_server:cast(Pid, {process_reminders, User, HolidayDate}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({process_reminders, User, HolidayDate}, State) ->
    lager:debug("Sending reminders for user ~p", [User]),

    Message = build_message(User, HolidayDate),
    Email = maps:get(email, User),
    {ok, Channels} = db_channel:list(Email),
    lists:foreach(fun(Channel) ->
                          remind_delivery:send(Channel, Message)
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
