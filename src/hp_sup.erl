-module(hp_sup).
-behaviour(supervisor).

-export([start_link/0,
         init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, { #{ strategy => one_for_one, intensity => 5, period => 1 },
         [#{
             id => remind_checker,
             start => {remind_checker, start_link, []},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => [remind_checker]
           },
          #{
             id => remind_router_sup,
             start => {remind_router_sup, start_link, []},
             restart => permanent,
             shutdown => 5000,
             type => supervisor,
             modules => [remind_router_sup]
           }]
       }}.
