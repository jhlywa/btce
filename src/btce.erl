-module(btce).

-export([start/0,

         %% query API
         ticker/1,
         trades/1,
         fee/1,
         depth/1,

         %% trade API (key required)
         info/0, info/1,
         orders/0, orders/1,
         cancel_order/1, cancel_order/2,
         trade/4, trade/5,

         %% key management API
         add_key/3, add_key/4,
         del_key/1,
         get_key/0, get_key/1,
         set_default_key/1,

         %% helpers
         trunc_amount/2,
         trunc_rate/2
        ]).

-include("btce.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%% BTCE types
-type pair()        :: btc_usd | btc_rur | btc_eur | ltc_btc | ltc_usd |
                       ltc_rur | nmc_btc | nvc_btc | usd_rur | eur_usd |
                       trc_btc | ppc_btc | ftc_btc | cnc_btc.
-type opts()        :: proplists:proplist().
-type http_method() :: get | post.
-type trade()       :: buy | sell.
-type error()       :: {error, term()}.

start() ->
    ok = start(btce).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% public BTC-E API (api key not required)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Retreive the fee for currency pair.
-spec fee(pair()) -> {ok, number()} | error().
fee(Pair) ->
    format_result(https(get, format_url(?FEE_URL, Pair))).

%% @doc Retreive the order book for currency pair.
-spec depth(pair()) -> {ok, proplists:proplist()} | error().
depth(Pair) ->
    format_result(https(get, format_url(?DEPTH_URL, Pair))).

%% @doc Retreive the ticker for currency pair.
-spec ticker(pair()) -> {ok, proplists:proplist()} | error().
ticker(Pair) ->
    format_result(https(get, format_url(?TICKER_URL, Pair))).

%% @doc Retreive most recent trades for currency pair.
-spec trades(pair()) -> {ok, proplists:proplist()} | error().
trades(Pair) ->
    format_result(https(get, format_url(?TRADES_URL, Pair))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private BTC-E API (api key required)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec info() -> {ok, proplists:proplist()} | error().
info() -> info([]).

-spec info(proplists:proplist()) -> {ok, proplists:proplist()} | error().
info(Opts) ->
    Params = [{method, 'getInfo'} | Opts],
    format_result(https(post, ?API_URL, Params)).

-spec orders() -> {ok, proplists:proplist()} | error().
orders() ->
    orders([]).

-spec orders(proplists:proplist()) -> {ok, proplists:proplist()} | error().
orders(Opts) ->
    Params = [{method, 'OrderList'} | Opts],
    format_result(https(post, ?API_URL, Params)).

-spec cancel_order(integer()) -> {ok, proplists:proplist()} | error().
cancel_order(OrderId) ->
    cancel_order(OrderId, []).

-spec cancel_order(integer(), proplists:proplist()) ->
                          {ok, proplists:proplist()} | error().
cancel_order(OrderId, Opts) ->
    format_result(https(post, ?API_URL, [{method, 'CancelOrder'},
                                         {order_id, OrderId} | Opts])).

-spec trade(pair(), trade(), float(), float()) ->
                   {ok, proplists:proplist()} | error().
trade(Pair, Action, Rate, Amount) ->
    trade(Pair, Action, Rate, Amount, []).

-spec trade(pair(), trade(), float(), float(), proplists:proplist()) ->
                   {ok, proplists:proplist()} | error().
trade(Pair, Action, Rate, Amount, Opts) ->
    format_result(https(post, ?API_URL, [{method, 'Trade'},
                                         {pair, Pair},
                                         {type, Action},
                                         {rate, Rate},
                                         {amount, Amount} | Opts])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API key management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec add_key(atom(), string(), string()) -> ok.
add_key(Name, PublicKey, PrivateKey)->
    add_key(Name, PublicKey, PrivateKey, []).

-spec add_key(atom(), string(), string(), proplists:proplist()) -> ok.
add_key(Name, PublicKey, PrivateKey, Opts)->
    btce_key_server:add_key(Name, PublicKey, PrivateKey, Opts).

-spec del_key(atom()) -> ok.
del_key(Name) ->
    btce_key_server:del_key(Name).

-spec get_key() -> {atom(), string(), string(), non_neg_integer()}.
get_key() ->
    btce_key_server:get_key().

-spec get_key(atom()) -> {atom(), string(), string(), non_neg_integer()}.
get_key(Name) ->
    btce_key_server:get_key(Name).

-spec set_default_key(atom()) -> ok.
set_default_key(Name) ->
    btce_key_server:set_default_key(Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start(atom()) -> ok.
start(App) ->
    case application:start(App) of
        {error, {not_started, Dep}}     -> start(Dep), start(App);
        {error, {already_started, Dep}} -> start(Dep), start(App);
        ok -> ok
    end.

-spec format_result(proplists:proplist()) ->
    {ok, float() | proplists:proplist()} | error().
format_result([{trade, Fee}])                   -> {ok, Fee};
format_result([{ticker, Ticker}])               -> {ok, Ticker};
format_result([{error, _}=Error])               -> Error;
format_result([{success, 0}, {error, _}=Error]) -> Error;
format_result([{success, 1}, {return, Result}]) -> {ok, Result};
format_result(Result)                           -> {ok, Result}.

-spec format_url(string(), pair()) -> string().
format_url(TemplateUrl, Pair) ->
    lists:flatten(io_lib:format(TemplateUrl, [Pair])).

-spec https(http_method(), string()) -> proplists:proplist().
https(get, Url) ->
    {ok, {_, _, Json}} = httpc:request(get, {Url, []}, [],
                                       [{body_format, binary}]),
    jsx:decode(Json, [{labels, atom}]).

-spec https(http_method(), string(), opts()) -> proplists:proplist().
https(post, Url, Opts) ->
    %% load key data from the key server (undefined key will retreive the
    %% default key
    ApiKey = proplists:get_value(api_key, Opts),
    Params = proplists:delete(api_key, Opts),
    {_, PublicKey, PrivateKey, Nonce} = btce_key_server:get_key(ApiKey),

    %% build headers and content of POST
    Content = encode_form_data([{nonce, Nonce} | Params]),
    Signature = crypto:hmac(sha512, PrivateKey, Content),
    Headers = [{"Sign", hex(Signature, 128)},
               {"Key", PublicKey}],

    Request = {Url, Headers, ?CONTENT_TYPE, Content},
    {ok, {_, _, Json}} = httpc:request(post, Request, [],
                                       [{body_format, binary}]),
    jsx:decode(Json, [{labels, atom}]).

-spec encode_form_data(proplists:proplist()) -> string().
encode_form_data(Params) ->
    Pair = proplists:get_value(pair, Params),
    Strings = [stringify(Pair, Param) || Param <- Params],
    string:join(Strings, "&").

-spec stringify(pair() | undefined, {atom(), atom() | number() | string()}) ->
                       string().
stringify(_, {method, Method}) ->
    lists:flatten(io_lib:format("method=~s", [Method]));
stringify(Pair, {amount, Amount}) ->
    lists:flatten(io_lib:format("amount=~p", [trunc_amount(Pair, Amount)]));
stringify(Pair, {rate, Rate}) ->
    lists:flatten(io_lib:format("rate=~p", [trunc_rate(Pair, Rate)]));
stringify(_, {K, V}) ->
    lists:flatten(io_lib:format("~p=~p", [K, V])).

-spec trunc_rate(pair(), number()) -> number().
trunc_rate(btc_usd, Rate) -> trunc(Rate, ?RATE_DECIMALS_BTC_USD);
trunc_rate(btc_rur, Rate) -> trunc(Rate, ?RATE_DECIMALS_BTC_RUR);
trunc_rate(btc_eur, Rate) -> trunc(Rate, ?RATE_DECIMALS_BTC_EUR);
trunc_rate(ltc_btc, Rate) -> trunc(Rate, ?RATE_DECIMALS_LTC_BTC);
trunc_rate(ltc_usd, Rate) -> trunc(Rate, ?RATE_DECIMALS_LTC_USD);
trunc_rate(ltc_rur, Rate) -> trunc(Rate, ?RATE_DECIMALS_LTC_RUR);
trunc_rate(nmc_btc, Rate) -> trunc(Rate, ?RATE_DECIMALS_NMC_BTC);
trunc_rate(nvc_btc, Rate) -> trunc(Rate, ?RATE_DECIMALS_NVC_BTC);
trunc_rate(usd_rur, Rate) -> trunc(Rate, ?RATE_DECIMALS_USD_RUR);
trunc_rate(eur_usd, Rate) -> trunc(Rate, ?RATE_DECIMALS_EUR_USD);
trunc_rate(trc_btc, Rate) -> trunc(Rate, ?RATE_DECIMALS_TRC_BTC);
trunc_rate(ppc_btc, Rate) -> trunc(Rate, ?RATE_DECIMALS_PPC_BTC);
trunc_rate(ftc_btc, Rate) -> trunc(Rate, ?RATE_DECIMALS_FTC_BTC);
trunc_rate(cnc_btc, Rate) -> trunc(Rate, ?RATE_DECIMALS_CNC_BTC).

-spec trunc_amount(pair(), number()) -> number().
trunc_amount(btc_usd, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_BTC_USD);
trunc_amount(btc_rur, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_BTC_RUR);
trunc_amount(btc_eur, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_BTC_EUR);
trunc_amount(ltc_btc, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_LTC_BTC);
trunc_amount(ltc_usd, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_LTC_USD);
trunc_amount(ltc_rur, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_LTC_RUR);
trunc_amount(nmc_btc, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_NMC_BTC);
trunc_amount(nvc_btc, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_NVC_BTC);
trunc_amount(usd_rur, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_USD_RUR);
trunc_amount(eur_usd, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_EUR_USD);
trunc_amount(trc_btc, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_TRC_BTC);
trunc_amount(ppc_btc, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_PPC_BTC);
trunc_amount(ftc_btc, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_FTC_BTC);
trunc_amount(cnc_btc, Amount) -> trunc(Amount, ?AMOUNT_DECIMALS_CNC_BTC).

-spec trunc(number(), pos_integer()) -> float().
trunc(Value, Length) ->
    Multiplier = math:pow(10, Length),
    trunc(Value * Multiplier) / Multiplier.

-spec hex(bitstring(), pos_integer()) -> string().
hex(Bin, Length) when is_bitstring(Bin) ->
    Bits = bit_size(Bin),
    <<Number:Bits>> = Bin,
    lists:flatten(io_lib:format("~*.16.0b", [Length, Number])).
