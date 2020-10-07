-module(email_channel).

-export([handle/4]).

handle(_User, _Date, Config, Message) ->
  FromEmail = <<"HolidayPinger <reminder@holidaypinger.com>">>,
  Subject = <<"Holiday reminder">>,
  Targets = maps:get(emails, Config),

  hp_email:send(Targets, FromEmail, Subject, Message),
  {ok, [Targets]}.
