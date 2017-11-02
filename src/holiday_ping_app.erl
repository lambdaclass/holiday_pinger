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
                                           {"/img/[...]", cowboy_static, {priv_dir, holiday_ping, "/ui/resources/public/img"}},
                                           {"/oauth/github", github_redirect_handler, []},
                                           {"/api/auth/github/code", github_callback_handler, []},
                                           {"/api/users", user_handler, []},
                                           {"/api/users/confirmation", confirmation_handler, []},
                                           {"/api/users/confirmation/code", confirmation_code_handler, []},
                                           {"/api/users/password", password_reset_handler, []},
                                           {"/api/users/password/code", password_reset_code_handler, []},
                                           {"/api/auth/token", token_handler, []},
                                           {"/api/channels", channel_list_handler, []},
                                           {"/api/channels/:name", channel_item_handler, []},
                                           {"/api/channels_detail", channel_detail_list_handler, []},
                                           {"/api/channels/:name/test", channel_test_handler, []},
                                           {"/api/channels/:channel/holidays", holidays_handler, []},
                                           {"/api/channels/:channel/reminders", sent_reminders_handler, []},
                                           {"/api/holidays/:country", country_holidays_handler, []},
                                           {'_', cowboy_static, {priv_file, holiday_ping, "/ui/resources/public/index.html"}}]}
                                   ]),
  start_cowboy(hp_config:get(protocol),
               [{env, [{dispatch, Dispatch}]},
                {middlewares, [cowboy_router, throttling_middleware, cowboy_handler]}]),
  hp_sup:start_link().

start_cowboy(http, Options) ->
  cowboy:start_http(my_http_listener, 100, [{port, hp_config:get(port)}], Options);
start_cowboy(https, Options) ->
  cowboy:start_https(my_http_listener, 100,
                     [{port, hp_config:get(port)},
                      {certfile, hp_config:get(certfile)},
                      {keyfile, hp_config:get(keyfile)}], Options).

stop(_State) ->
  ok.
