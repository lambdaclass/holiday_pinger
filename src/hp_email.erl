-module(hp_email).

-export([send/4,
         send_email_verification/2]).

send(ToEmails, FromEmail, Subject, Body) when not is_list(ToEmails)->
  send([ToEmails], FromEmail, Subject, Body);
send(ToEmails, FromEmail, Subject, Body) ->
  lager:debug(<<"Sending amazon SES emails to ~p">>, [ToEmails]),

  case hp_config:get(email_enabled) of
    true ->
      lists:foreach(fun(Email) ->
                        erlcloud_ses:send_email(Email, Body, Subject, FromEmail, [])
                    end, ToEmails);
    _ ->
      ok
  end.

send_email_verification(Email, Code) ->
  From = <<"HolidayPing <holidayping@lambdaclass.com>">>,
  Subject = <<"HolidayPing email confirmation.">>,
  Body = <<"Hey there!\n\n"
           "Thanks for choosing at HolidayPing, "
           "please click on the link below to finish the registration process:\n\n"
           "https://holidayping.lambdaclass.com/register/confirm/code?code=", Code/binary,
           "&email=", Email/binary,
           "\n\nThanks,\nThe HolidayPing team.">>,

  send(Email, From, Subject, Body).
