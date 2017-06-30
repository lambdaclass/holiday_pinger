-module(req_utils).

-export([error_response/3,
         error_response/2,
         success_response/2]).

success_response(Data, Req) ->
    Body = hp_json:encode(Data),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {true, Req2, []}.

error_response(Message, Req) ->
    error_response(400, Message, Req).
error_response(Status, Message, Req) ->
    Body = hp_json:encode(#{message => Message}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {ok, Req3} = cowboy_req:reply(Status, [], Body, Req2),
    {halt, Req3, []}.
