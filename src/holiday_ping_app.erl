-module(holiday_ping_app).

-behaviour(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    pgapp:connect(hp_config:get(pg_options)),

    Dispatch = cowboy_router:compile([
                                      {'_', [{"/", cowboy_static, {priv_file, holiday_ping, "/ui/index.html"}},
                                             {"/js/[...]", cowboy_static, {priv_dir, holiday_ping, "/ui/js"}},
                                             {"/api/users", user_handler, []},
                                             {"/api/auth/token", token_handler, []},
                                             {"/api/channels", channel_list_handler, []},
                                             {"/api/channels/:name", channel_detail_handler, []}]}
                                     ]),
    cowboy:start_http(my_http_listener, 100, [{port, hp_config:get(port)}],
                      [{env, [{dispatch, Dispatch}]}]
                     ),
    hp_sup:start_link().

stop(_State) ->
    ok.
