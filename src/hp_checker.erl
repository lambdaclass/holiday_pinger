-module(hp_checker).

-behaviour(gen_server).

-export([start_link/0,

         check_holidays/0,

         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
%% TODO make configurable
-define(INTERVAL, 1000 * 60 * 60).

%% triggered by the interval, export for manual tests
check_holidays() ->
    io:format("Running holiday checker.~n"),
    Countries = hp_holiday_db:countries_with_holiday(),
    Users = hp_user_db:get_from_countries(Countries),
    %% for now remind when we're already in the holiday
    HolidayDate = erlang:date(),
    lists:foreach(fun (User) -> hp_reminder:send(User, HolidayDate) end, Users),
    ok.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, _} = timer:send_interval(?INTERVAL, {check_holidays}),
    {ok, no_state}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({check_holidays}, State) ->
    check_holidays(),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
