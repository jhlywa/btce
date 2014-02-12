BTCE
====

btce is an implementation of the BTC-e API for Erlang systems.  The API is
divided into three logical categories: the query API, the trading API, and
the key management API.

## Important Info
* Due to httpc dependencies (ssl, inets, crypto) the btce library must be
started prior to use (`btce:start()`).

* A valid BTC-e api key is required for all _trading_ API calls.  See
[BTC-e](https://btc-e.com/profile#api_keys) for more information.

* The BTC-e trading API uses an incrementing nonce to avoid replay attacks.
Concurrently executed trade API calls should use unique API keys to avoid
race conditions.

## Query API Example
```erlang
1> btce:start().
ok
2> btce:ticker(btc_usd).
{ok,[{high,101.699},
     {low,92.198},
     {avg,96.9485},
     {vol,685126.89766},
     {vol_cur,7046.95461},
     {last,97.894},
     {buy,97.893},
     {sell,97.498},
     {updated,1372125701},
     {server_time,1372125703}]}
```

## Trade API Example
```erlang
1> btce:start().
ok
2> %% stores api key in a DETS table, only needs to be done once .... ever
2> btce:add_key(my_api_key, "PUBLIC KEY", "PRIVATE KEY", [{default, true}]).
ok
3> %% trade API calls use the default key (unless overridden)
3> btce:info()
{ok, [{funds,
       [{usd,210.76262},
        {rur,0},
        {eur,0},
        {btc,0.},
        {ltc,200.179},
        {nmc,0},
        {nvc,0},
        {trc,0},
        {ppc,0},
        {ftc,0},
        {cnc,0}]},
      {rights,[{info,1},{trade,1},{withdraw,0}]},
      {transaction_count,8392},
      {open_orders,5},
      {server_time,1368442771}]

4> %% buy 1.23 BTC at 90.01 USD using a different API key
4> btce:trade(btc_usd, buy, 90.01, 1.23, [{api_key, different_key}]).
{ok,[{received,1.23},
     {remains,0},
     {order_id,0},
     {funds,
      [{usd,100.05032},
       {rur,0},
       {eur,0},
       {btc,1.23},
       {ltc,200.179},
       {nmc,0},
       {nvc,0},
       {trc,0},
       {ppc,0},
       {ftc,0},
       {cnc,0}]}]}
```

## Types ##

```erlang
pair()   = btc_usd | btc_rur | btc_eur | ltc_btc | ltc_usd |
           ltc_rur | ltc_eur | nmc_btc | nmc_usd | nvc_btc |
           nvc_usd | usd_rur | eur_usd | eur_rur | trc_btc |
           ppc_btc | ppc_usd | ftc_btc | xpm_btc.
rate()   = float().
amount() = float().
```

## Query API
### ticker/1
Retreive ticker for given currency pair.

```erlang
ticker(pair()) -> {error, term()} | {ok,[{high, float()},
                                         {low, float()},
                                         {avg, float()},
                                         {vol, float()},
                                         {vol_cur, float()},
                                         {last, float()},
                                         {buy, float()},
                                         {sell, float()},
                                         {updated, non_neg_integer()},
                                         {server_time,non_neg_integer()}]}.
```
### depth/1
Retreive the order book for currency pair.
```erlang
depth(pair()) -> {error, term()} | {ok,[{asks, list([float(), float()])},
                                        {bids, list([float(), float()])}]}.
```

### fee/1
Retreive the exchange fee for currency pair.
```erlang
fee(pair()) -> {error, term()} | {ok, float()}.
```

### trades/1
Returns the most recent trades for currency pair.
```erlang
trades(pair()) -> {error, term()} | {ok, list([{date, non_neg_integer()},
                                               {price, float()},
                                               {amount, float()},
                                               {tid, non_neg_integer()},
                                               {price_currency, binary()},
                                               {item, binary()},
                                               {trade_type,<<"ask">> |
                                                           <<"bid">>}])}.
```

## Trade API
### info/0, info/1
Returns profile information including current funds, key privledges, and order
counts.

### trade/4, trade/5
Buy or sell a currency pair at a given rate and amount.
```erlang
trade(pair(), buy | sell, rate(), amount()) -> {error, term()} | {ok, ...}.
```

### orders/0, orders/1
Return orders opened, filled, or canceled using the provided API key.

### cancel_order/1, cancel_order/2
Cancel an order.

## Key Management API
### add_key/3, add_key/4,
### del_key/1,
### get_key/0, get_key/1,
### set_default_key/1,

## Helper Methods
### start/0
### pip/1
Return the smallest price (or rate) change for a given currency pair.
```erlang
pip(pair()) -> float().
```

## TODO
- update to support latest currency pairs
- better documentation
- add remaining trade API methods (trans_history, trade_history)
- add more tests (use PropEr)
