-module(remind_delivery).
-behaviour(gen_server).

%%% This module sends a message through a channel with the received configuration.

-export([start_link/0,

         send/2,

         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

send(#{type := Type, configuration := Config}, Message) ->
    {ok, Pid} = supervisor:start_child(remind_delivery_sup, []),
    gen_server:cast(Pid, {deliver_reminder, Type, Config, Message}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

%% Pass the message to the proper channel handler
handle_cast({deliver_reminder, Type, Config, Message}, State) ->
    Handler = get_handler(Type),
    Handler:handle(Config, Message),
    {noreply, State};
handle_cast(Request, State) ->
    lager:warning("Unknown message: ~p", [Request]),
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internal
get_handler(slack) -> slack_channel;
get_handler(console) -> console_channel;
get_handler(ets) -> ets_channel.
