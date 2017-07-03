-module(db_channel).

-export([get_user_channels/1]).

get_user_channels(_User) ->
    [#{
        id => 1,
        name => <<"My slack channel">>,
        type => slack
      },
     #{
        id => 2,
        name => <<"My console channel">>,
        type => console
      }].
