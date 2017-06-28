-module(db_channel).

-export([get_user_channels/1]).

get_user_channels(User) ->
    [#{
        id => 1,
        name => <<"My slack channel">>,
        type => slack
      }].
