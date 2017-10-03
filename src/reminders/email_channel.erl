-module(email_channel).

-export([handle/4]).

handle(User, _Date, Config, Message) ->
  UserName = maps:get(name, User),
  FromEmail = <<UserName/binary, " <holidayping@lambdaclass.com>">>,
  Subject = <<"Holiday reminder">>,
  Targets = maps:get(emails, Config),

  hp_email:send(Targets, FromEmail, Subject, Message),
  {ok, [Targets]}.
