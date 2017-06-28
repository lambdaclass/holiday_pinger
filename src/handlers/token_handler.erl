-module(token_handler).

-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         to_json/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

is_authorized(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {ok, {<<"basic">>, {Email, Password}}, Req2} ->
            case db_user:authenticate(Email, Password) of
                {ok, User} ->
                    {true, Req2, #{user => User}};
                _ -> {{false, <<"Basic realm=\"holidayping\"">>}, Req2, State}
            end;
        _ -> {{false, <<"Basic realm=\"holidayping\"">>}, Req, State}
    end.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State = #{user := User}) ->
    {ok, Token} = hp_auth_tokens:encode(User),
    Body = hp_json:encode(#{access_token => Token}),
    {Body, Req, State}.
