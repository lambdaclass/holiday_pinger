-module(holiday_ping_app).

-behaviour(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    pgapp:connect(hp_config:get(pg_options)),

    Dispatch = cowboy_router:compile([
                                      {'_', [{"/favicon.ico", cowboy_static, {priv_file, holiday_ping, "/ui/resources/public/favicon.ico"}},
                                             {"/js/[...]", cowboy_static, {priv_dir, holiday_ping, "/ui/resources/public/js"}},
                                             {"/css/[...]", cowboy_static, {priv_dir, holiday_ping, "/ui/resources/public/css"}},
                                             {"/oauth/github", github_redirect_handler, []},
                                             {"/api/auth/github", github_auth_handler, []},
                                             {"/api/users", user_handler, []},
                                             {"/api/auth/token", token_handler, []},
                                             {"/api/channels", channel_list_handler, []},
                                             {"/api/holidays", holidays_handler, []},
                                             {"/api/reminders", reminder_handler, []},
                                             {"/api/channels/:name", channel_detail_handler, []},
                                             {"/api/channels/:name/test", channel_test_handler, []},
                                             {'_', cowboy_static, {priv_file, holiday_ping, "/ui/resources/public/index.html"}}]}
                                     ]),
    cowboy:start_http(my_http_listener, 100, [{port, hp_config:get(port)}],
                      [{env, [{dispatch, Dispatch}]}]
                     ),
    hp_sup:start_link().

stop(_State) ->
    ok.
