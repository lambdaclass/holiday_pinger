-module(hp_checker).
-behaviour(gen_server).

-export([start_link/0,

         force_holidays/0,

         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% TODO make configurable
-define(INTERVAL, 1000 * 60 * 60 * 24).

%% for testing, foce the checker to send reminders
force_holidays() ->
    check_holidays({2017, 1, 1}),
    ok.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, _} = timer:send_interval(?INTERVAL, check_holidays),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(check_holidays, State) ->
    %% for now remind when we're already in the holiday
    HolidayDate = erlang:date(),
    check_holidays(HolidayDate),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% internal
check_holidays(HolidayDate) ->
    io:format("Running holiday checker.~n"),
    Countries = hp_holiday_db:countries_with_holiday(HolidayDate),
    Users = hp_user_db:get_from_countries(Countries),
    lists:foreach(fun (User) -> hp_reminder:send(User, HolidayDate) end, Users),
    ok.
