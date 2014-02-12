-module(btce_tests).

-include_lib("eunit/include/eunit.hrl").

stringify_test() ->
    Args = [{btc_usd, {method, 'Trade'}, "method=Trade"},
            {btc_usd, {method, 'getInfo'}, "method=getInfo"},
            {btc_usd, {method, 'OrderList'}, "method=OrderList"},
            {btc_usd, {method, 'CancelOrder'}, "method=CancelOrder"},

            {btc_usd, {amount, 1}, "amount=1.00000000"},
            {btc_usd, {amount, 1.0e3}, "amount=1000.00000000"},
            {btc_usd, {amount, 1234.123456789}, "amount=1234.12345678"},
            {btc_rur, {amount, 1234.123456789}, "amount=1234.12345678"},
            {btc_eur, {amount, 1234.123456789}, "amount=1234.12345678"},
            {ltc_btc, {amount, 1234.123456789}, "amount=1234.12345678"},
            {ltc_usd, {amount, 1234.123456789}, "amount=1234.12345678"},
            {ltc_rur, {amount, 1234.123456789}, "amount=1234.12345678"},
            {nmc_btc, {amount, 1234.123456789}, "amount=1234.12345678"},
            {nmc_usd, {amount, 1234.123456789}, "amount=1234.12345678"},
            {nvc_btc, {amount, 1234.123456789}, "amount=1234.12345678"},
            {nvc_usd, {amount, 1234.123456789}, "amount=1234.12345678"},
            {usd_rur, {amount, 1234.123456789}, "amount=1234.12345678"},
            {eur_usd, {amount, 1234.123456789}, "amount=1234.12345678"},
            {eur_rur, {amount, 1234.123456789}, "amount=1234.12345678"},
            {trc_btc, {amount, 1234.123456789}, "amount=1234.12345678"},
            {ppc_btc, {amount, 1234.123456789}, "amount=1234.12345678"},
            {ppc_usd, {amount, 1234.123456789}, "amount=1234.12345678"},
            {ftc_btc, {amount, 1234.123456789}, "amount=1234.12345678"},
            {xpm_btc, {amount, 1234.123456789}, "amount=1234.12345678"},

            {btc_usd, {rate, 1}, "rate=1.000"},
            {btc_usd, {rate, 1000}, "rate=1000.000"},
            {btc_usd, {rate, 1234.123456789}, "rate=1234.123"},
            {btc_rur, {rate, 1234.123456789}, "rate=1234.12345"},
            {btc_eur, {rate, 1234.123456789}, "rate=1234.12345"},
            {ltc_btc, {rate, 1234.123456789}, "rate=1234.12345"},
            {ltc_btc, {rate, 0.03142},        "rate=0.03142"},
            {ltc_usd, {rate, 1234.123456789}, "rate=1234.123456"},
            {ltc_rur, {rate, 1234.123456789}, "rate=1234.12345"},
            {nmc_btc, {rate, 1234.123456789}, "rate=1234.12345"},
            {nmc_usd, {rate, 1234.123456789}, "rate=1234.123"},
            {nvc_btc, {rate, 1234.123456789}, "rate=1234.12345"},
            {nvc_usd, {rate, 1234.123456789}, "rate=1234.123"},
            {usd_rur, {rate, 1234.123456789}, "rate=1234.12345"},
            {eur_usd, {rate, 1234.123456789}, "rate=1234.12345"},
            {eur_rur, {rate, 1234.123456789}, "rate=1234.12345"},
            {trc_btc, {rate, 1234.123456789}, "rate=1234.12345"},
            {ppc_btc, {rate, 1234.123456789}, "rate=1234.12345"},
            {ppc_usd, {rate, 1234.123456789}, "rate=1234.123"},
            {ftc_btc, {rate, 1234.123456789}, "rate=1234.12345"},
            {xpm_btc, {rate, 1234.123456789}, "rate=1234.12345"}
           ],

    [?assertEqual(btce:stringify(Pair, Arg), Result) ||
        {Pair, Arg, Result} <- Args].

pip_test() ->
    PipTests = [{btc_usd, 0.001},
                {btc_rur, 0.00001},
                {btc_eur, 0.00001},
                {ltc_btc, 0.00001},
                {ltc_usd, 0.000001},
                {ltc_rur, 0.00001},
                {nmc_btc, 0.00001},
                {nmc_usd, 0.001},
                {nvc_btc, 0.00001},
                {nvc_usd, 0.001},
                {usd_rur, 0.00001},
                {eur_usd, 0.00001},
                {eur_rur, 0.00001},
                {trc_btc, 0.00001},
                {ppc_btc, 0.00001},
                {ppc_usd, 0.001},
                {ftc_btc, 0.00001},
                {xpm_btc, 0.00001}],
    [?assertEqual(btce:pip(Pair), Pip) || {Pair, Pip} <- PipTests].

ticker_test() ->
    do_ticker_test(code:which(meck)).

do_ticker_test(non_existing) ->
    ?debugMsg("meck not found, skipping ticker_test");
do_ticker_test(_) ->
    meck:new(httpc),
    meck:expect(httpc, request,
                fun(_, _, _, _) ->
                        {ok,{{"HTTP/1.1",200,"OK"},
                             [ignore],
                             <<"{\"ticker\":{\"high\":101.699,\"low\":92.198,"
                               "\"avg\":96.9485,\"vol\":685126.89766,"
                               "\"vol_cur\":7046.95461,\"last\":97.894,"
                               "\"buy\":97.893,\"sell\":97.498,"
                               "\"updated\":1372125701,"
                               "\"server_time\":1372125703}}">>}}
                end),

    {ok,[{high,101.699},
         {low,92.198},
         {avg,96.9485},
         {vol,685126.89766},
         {vol_cur,7046.95461},
         {last,97.894},
         {buy,97.893},
         {sell,97.498},
         {updated,1372125701},
         {server_time,1372125703}]} = btce:ticker(btc_usd),

    meck:unload(httpc).

fee_test() ->
    do_fee_test(code:which(meck)).

do_fee_test(non_existing) ->
    ?debugMsg("meck not found, skipping fee_test");
do_fee_test(_) ->
    meck:new(httpc),
    meck:expect(httpc, request,
                fun(_, _, _, _) ->
                        {ok,{{"HTTP/1.1",200,"OK"},
                             [ignore],
                             <<"{\"trade\":0.2}">>}}
                end),

    {ok, 0.2} = btce:fee(btc_usd),

    meck:unload(httpc).

depth_test() ->
    do_depth_test(code:which(meck)).

do_depth_test(non_existing) ->
    ?debugMsg("meck not found, skipping depth_test");
do_depth_test(_) ->
    meck:new(httpc),
    meck:expect(httpc, request,
                fun(_, _, _, _) ->
                        {ok,{{"HTTP/1.1",200,"OK"},
                             [ignore],
                             <<"{\"asks\": [[97.9, 14.11],"
                                          "[97.956, 0.01115834]],"
                               "\"bids\": [[97.504,0.13121176],"
                               "[97.502,0.09091856]]}">>}}
                end),

    {ok,[{asks,[[97.9,14.11],
                [97.956,0.01115834]]},
         {bids,[[97.504,0.13121176],
                [97.502,0.09091856]]}]} = btce:depth(btc_usd),

    meck:unload(httpc).


