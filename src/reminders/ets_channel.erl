-module(ets_channel).

-export([handle/4,
         init_table/1,
         get_reminders/2]).

%% channel that saves the notifications to an ETS for testing

init_table(TableId) ->
  ets:new(TableId, [bag, public, named_table]).

get_reminders(TableId, Email) ->
  ets:lookup(TableId, Email).

handle(_User, _Date, Config, Message) ->
  #{email := Email,
    table_id := TableId} = Config,
  ets:insert(erlang:binary_to_existing_atom(TableId, latin1), {Email, Message}),
  ok.
