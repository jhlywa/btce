%%% ---------------------------------------------------------------------------
%%% btce: BTC-e API for Erlang systems
%%%
%%% Copyright (c) 2013 Jeff Hlywa (jhlywa@gmail.com)
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice,
%%%    this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ---------------------------------------------------------------------------

-module(btce).

%% query API
-export([ticker/1]).
-export([trades/1]).
-export([depth/1]).
-export([fee/1]).

%% trade API (key required)
-export([info/0]).
-export([info/1]).
-export([orders/0]).
-export([orders/1]).
-export([trade/4]).
-export([trade/5]).
-export([cancel_order/1]).
-export([cancel_order/2]).

%% key management API
-export([add_key/3]).
-export([add_key/4]).
-export([del_key/1]).
-export([get_key/0]).
-export([get_key/1]).
-export([set_default_key/1]).

%% helpers
-export([pairs/0]).
-export([pip/1]).
-export([start/0]).

%% supported currency pairs
-define(PAIRS, [btc_usd, btc_rur, btc_eur, btc_cnh, btc_gbp, ltc_btc, ltc_usd,
                ltc_rur, ltc_eur, ltc_cnh, ltc_gbp, nmc_btc, nmc_usd, nvc_btc,
                nvc_usd, usd_rur, eur_usd, eur_rur, usd_cnh, gbp_usd, trc_btc,
                ppc_btc, ppc_usd, ftc_btc, xpm_btc]).

%% URL's used with query API
-define(TICKER_URL, "https://btc-e.com/api/2/~p/ticker").
-define(DEPTH_URL, "https://btc-e.com/api/2/~p/depth").
-define(FEE_URL, "https://btc-e.com/api/2/~p/fee").
-define(TRADES_URL, "https://btc-e.com/api/2/~p/trades").

%% URL for trade API
-define(API_URL, "https://btc-e.com/tapi").

%% used when POST'ing data
-define(CONTENT_TYPE, "application/x-www-form-urlencoded").

%% max decimal places used when submitting a trade ... {rate, amount}
-define(DECIMALS(P),
        case P of
            btc_usd -> {3, 8};
            btc_rur -> {5, 8};
            btc_eur -> {5, 8};
            btc_cnh -> {2, 8};
            btc_gbp -> {4, 8};
            ltc_btc -> {5, 8};
            ltc_usd -> {6, 8};
            ltc_rur -> {5, 8};
            ltc_eur -> {3, 8};
            ltc_cnh -> {2, 8};
            ltc_gbp -> {3, 8};
            nmc_btc -> {5, 8};
            nmc_usd -> {3, 8};
            nvc_btc -> {5, 8};
            nvc_usd -> {3, 8};
            usd_rur -> {5, 8};
            eur_usd -> {5, 8};
            eur_rur -> {5, 8};
            usd_cnh -> {4, 8};
            gbp_usd -> {4, 8};
            trc_btc -> {5, 8};
            ppc_btc -> {5, 8};
            ppc_usd -> {3, 8};
            ftc_btc -> {5, 8};
            xpm_btc -> {5, 8}
        end).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%% BTCE types
-type pair()        :: btc_usd | btc_rur | btc_eur | btc_cnh | btc_gbp |
                       ltc_btc | ltc_usd | ltc_rur | ltc_eur | ltc_cnh |
                       ltc_gbp | nmc_btc | nmc_usd | nvc_btc | nvc_usd |
                       usd_rur | eur_usd | eur_rur | usd_cnh | gbp_usd |
                       trc_btc | ppc_btc | ppc_usd | ftc_btc | xpm_btc.

-type opts()        :: proplists:proplist().
-type http_method() :: get | post.
-type trade()       :: buy | sell.
-type error()       :: {error, term()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% public BTC-E API (api key not required)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Retrieve the fee for currency pair.
-spec fee(pair()) -> {ok, number()} | error().
fee(Pair) ->
    format_result(https(get, format_url(?FEE_URL, Pair))).

%% @doc Retrieve the order book for currency pair.
-spec depth(pair()) -> {ok, proplists:proplist()} | error().
depth(Pair) ->
    format_result(https(get, format_url(?DEPTH_URL, Pair))).

%% @doc Retrieve the ticker for currency pair.
-spec ticker(pair()) -> {ok, proplists:proplist()} | error().
ticker(Pair) ->
    format_result(https(get, format_url(?TICKER_URL, Pair))).

%% @doc Retrieve most recent trades for currency pair.
-spec trades(pair()) -> {ok, proplists:proplist()} | error().
trades(Pair) ->
    format_result(https(get, format_url(?TRADES_URL, Pair))).

%% @doc Return the supported currency pairs.
pairs() -> ?PAIRS.

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
%%% helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    ok = start(btce).

%% @doc Return the smallest rate increment for the currency pair.
pip(Pair) ->
    {Decimals, _} = ?DECIMALS(Pair),
    math:pow(10, -Decimals).

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
    {_, Decimals} = ?DECIMALS(Pair),
    lists:flatten(io_lib:format("amount=~s", [digits(Amount, Decimals)]));
stringify(Pair, {rate, Rate}) ->
    {Decimals, _}  = ?DECIMALS(Pair),
    lists:flatten(io_lib:format("rate=~s", [digits(Rate, Decimals)]));
stringify(_, {K, V}) ->
    lists:flatten(io_lib:format("~p=~p", [K, V])).

%% @doc Return a string representation of a floating point number with a
%% specific number of decimal places.  This is next to impossible to do
%% without the help of mochinum (see
%% http://bob.ippoli.to/archives/2007/12/17/printing-floats-with-erlang/)
%% for more info.
-spec digits(number(), pos_integer()) -> string().
digits(Value, Decimals) ->
    [Integral, Fractional] = string:tokens(mochinum:digits(Value * 1.0), "."),
    Integral ++ "." ++ string:left(Fractional, Decimals, $0).

-spec hex(bitstring(), pos_integer()) -> string().
hex(Bin, Length) when is_bitstring(Bin) ->
    Bits = bit_size(Bin),
    <<Number:Bits>> = Bin,
    lists:flatten(io_lib:format("~*.16.0b", [Length, Number])).
