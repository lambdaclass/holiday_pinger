-module(remind_router_sup).
-behaviour(supervisor).

-export([start_link/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { #{ strategy => simple_one_for_one, intensity => 5, period => 1 },
           [#{
               id => remind_router,
               start => {remind_router, start_link, []},
               restart => transient,
               shutdown => 5000,
               type => worker,
               modules => [remind_router]
             }]
         }}.
