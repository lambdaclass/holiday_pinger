-module(holiday_ping_app).

-behaviour(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    %% FIXME make configurable
    pgapp:connect([{size, 10},
                   {database, "holiday_ping"},
                   {username, "postgres"},
                   {password, "example"}]),

    Dispatch = cowboy_router:compile([
                                      {'_', [{"/", cowboy_static, {priv_file, holiday_ping, "index.html"}},
                                             {"/assets/[...]", cowboy_static, {priv_dir, holiday_ping, ""}},
                                             {"/api/users", user_handler, []},
                                             {"/api/auth/tokens", token_handler, []},
                                             {"/api/channels", channel_list_handler, []},
                                             {"/api/channels/:id", [{id, int}], channel_detail_handler, []}]}
                                     ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8001}],
                      [{env, [{dispatch, Dispatch}]}]
                     ),
    hp_sup:start_link().

stop(_State) ->
    ok.
