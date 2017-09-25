-module(email_channel).

-export([handle/4]).

handle(User, _Date, Config, Message) ->
  UserName = maps:get(name, User),
  Subject = <<"Holiday reminder">>,
  Targets = maps:get(emails, Config),

  lager:debug(<<"Sending amazon SES emails to ~p">>, [Targets]),
  lists:foreach(fun(Email) ->
                    erlcloud_ses:send_email(Email,
                                            Message,
                                            Subject,
                                            <<UserName/binary, " <holidayping@lambdaclass.com>">>, [])
                end, Targets).
