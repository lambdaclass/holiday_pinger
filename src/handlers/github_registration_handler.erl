-module(github_registration_handler).

%%% REST handler that receives the missing profile data required to finish
%%% the registration of a user coming from GitHub

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         is_authorized/2,
         from_json/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #{}}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

is_authorized(Req, State) ->
    Fail = {false, <<"Bearer realm=\"holidayping\"">>},
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {ok, {<<"bearer">>, Token}, Req2} ->
            case hp_auth:registration_token_decode(Token) of
                {ok, UserData} ->
                    {true, Req2, State#{user_data => UserData}};
                _ -> {Fail, Req2, State}
            end;
        _ -> {Fail, Req, State}
    end.

from_json(Req, #{user_data := UserData} = State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),

    case hp_json:decode(Body) of
        #{country := Country} ->
            LCountry = string:lowercase(Country),
            register_user(UserData, LCountry),
            Token = build_holiday_access_token(UserData, LCountry),
            RespBody = hp_json:encode(#{access_token => Token}),
            Req3 = cowboy_req:set_resp_body(RespBody, Req2),
            {true, Req3, State};
        _ ->
            req_utils:error_response(<<"Missing required fields">>, Req2)
    end.

%%% internal

register_user(#{<<"email">> := Email, <<"name">> := Name}, Country) ->
    %% only attempt to create it if it's not already registered
    case db_user:get(Email) of
        {error, not_found} ->
            {ok, _} = db_user:create_github(Email, Name, Country),

            %% TODO this should go in a user model eventually, instead of the API handler
            ok = db_holiday:set_default_holidays(Email, Country),
            ok = db_reminder:set_default_reminder_config(Email);
        {ok, _} ->
            ok
    end.

build_holiday_access_token(UserData, Country) ->
    {ok, Token} = hp_auth:access_token_encode(UserData#{country => Country}),
    Token.
