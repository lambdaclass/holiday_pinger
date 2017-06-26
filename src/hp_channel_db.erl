-module(hp_channel_db).

-export([get_user_channels/1]).

get_user_channels(User) ->
    [#{
        id => 1,
        name => <<"My console channel">>,
        type => console
      }].
