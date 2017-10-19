-module(email_channel).

-export([handle/4]).

handle(_User, _Date, Config, Message) ->
  FromEmail = <<"HolidayPing <holidayping@lambdaclass.com>">>,
  Subject = <<"Holiday reminder">>,
  Targets = maps:get(emails, Config),

  hp_email:send(Targets, FromEmail, Subject, Message),
  {ok, [Targets]}.
