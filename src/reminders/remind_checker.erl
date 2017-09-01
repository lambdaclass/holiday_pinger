-module(remind_checker).
-behaviour(gen_server).

%%% This module checks periodically which users have reminders to be sent,
%%% according to their holiday configuration, and calls remind_router with the user
%%% and holiday data.

-export([start_link/0,

         force_holidays/0,
         force_holidays/1,

         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Interval = hp_config:get(checker_interval),
    {ok, _} = timer:send_interval(Interval, check_holidays),
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

%% for testing, foce the checker to send reminders
force_holidays() ->
  force_holidays({2017, 1, 1}).
force_holidays(Date) ->
  check_holidays(Date),
  ok.

%%% internal
check_holidays(HolidayDate) ->
    lager:info("Running holiday checker."),
    {ok, Users} = db_holiday:users_with_holiday(HolidayDate),
    lists:foreach(fun (User) -> remind_router:send(User, HolidayDate) end, Users),
    ok.
