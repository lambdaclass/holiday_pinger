-module(remind_delivery_sup).
-behaviour(supervisor).

%%% simple_one_for_one supervisor whose workers send messages through specific channels

-export([start_link/0,
         init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, { #{ strategy => simple_one_for_one, intensity => 5, period => 1 },
         [#{
             id => remind_delivery,
             start => {remind_delivery, start_link, []},
             restart => transient,
             shutdown => 5000,
             type => worker,
             modules => [remind_delivery]
           }]
       }}.
