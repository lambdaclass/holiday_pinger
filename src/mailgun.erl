-module(mailgun).

-export([send_email/5]).

send_email(Email, FullBody, Subject, FromEmail, []) ->
  MailgunDomain = list_to_binary(os:getenv("MAILGUN_DOMAIN")),
  MailgunKey = list_to_binary(os:getenv("MAILGUN_KEY")),
  Url = <<"https://api.mailgun.net/v3/", MailgunDomain/binary, "/messages">>,
  Options = [{basic_auth, {<<"api">>, MailgunKey}}],
  [{html, Body}] = FullBody,
  Payload = {form, [{to, Email},
                    {from, FromEmail},
                    {subject, Subject},
                    {html, Body}]},
  case hackney:request(post, Url, [], Payload, Options) of
    {ok, 200, _, _} ->
      ok;
    {ok, StatusCode, _, _} ->
      lager:error(<<"Mailgun API call failed with ~p~nURL: ~p~nPayload: ~p~nOptions: ~p~n">>, [StatusCode, Url, Payload, Options]);
    {error, Reason} ->
      lager:error(<<"Hackney request failed ~p~n">>, [Reason])
  end.
