-module(req_utils).

-export([error_response/3,
         error_response/2,
         success_response/2,
         is_authorized/3]).

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

is_authorized(bearer, Req, State) ->
    Fail = {false, <<"Bearer realm=\"holidayping\"">>},
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {ok, {<<"bearer">>, Token}, Req2} ->
            case hp_auth:token_decode(Token) of
                {ok, #{<<"email">> := User}} ->
                    {true, Req2, State#{user => User}};
                _ -> {Fail, Req2, State}
            end;
        _ -> {Fail, Req, State}
    end;

is_authorized(basic, Req, State) ->
    Fail = {false, <<"Basic realm=\"holidayping\"">>},
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {ok, {<<"basic">>, {Email, Password}}, Req2} ->
            case hp_auth:authenticate(Email, Password) of
                {ok, User} ->
                    {true, Req2, State#{user => User}};
                _ -> {Fail, Req2, State}
            end;
        _ -> {Fail, Req, State}
    end.
