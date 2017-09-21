-module(country_holidays_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         to_json/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {Country, Req2} = cowboy_req:binding(country, Req),
  Country2 = string:lowercase(Country),
  Country3 = binary:replace(Country2, <<"+">>, <<" ">>),
  State = #{country => Country3},
  {ok, Req2, State}.

is_authorized(Req, State) ->
  req_utils:is_authorized(bearer, Req, State).

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>],
   Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State = #{country := Country}) ->
  {ok, Holidays} = db_holiday:holidays_of_country(Country),
  Body = hp_json:encode(Holidays),
  {Body, Req, State}.
