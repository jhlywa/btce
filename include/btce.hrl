%% define supported currency pairs
-define(PAIRS, [btc_usd, btc_rur, btc_eur, btc_cnh, btc_gbp, ltc_btc, ltc_usd,
                ltc_rur, ltc_eur, ltc_cnh, ltc_gbp, nmc_btc, nmc_usd, nvc_btc,
                nvc_usd, usd_rur, eur_usd, eur_rur, usd_cnh, gbp_usd, trc_btc,
                ppc_btc, ppc_usd, ftc_btc, xpm_btc]).

%% public api urls
-define(TICKER_URL, "https://btc-e.com/api/2/~p/ticker").
-define(DEPTH_URL, "https://btc-e.com/api/2/~p/depth").
-define(FEE_URL, "https://btc-e.com/api/2/~p/fee").
-define(TRADES_URL, "https://btc-e.com/api/2/~p/trades").

%% private api url
-define(API_URL, "https://btc-e.com/tapi").

%% max decimal places for rates
-define(RATE_DECIMALS_BTC_USD, 3).
-define(RATE_DECIMALS_BTC_RUR, 5).
-define(RATE_DECIMALS_BTC_EUR, 5).
-define(RATE_DECIMALS_BTC_CNH, 2).
-define(RATE_DECIMALS_BTC_GBP, 4).
-define(RATE_DECIMALS_LTC_BTC, 5).
-define(RATE_DECIMALS_LTC_USD, 6).
-define(RATE_DECIMALS_LTC_RUR, 5).
-define(RATE_DECIMALS_LTC_EUR, 3).
-define(RATE_DECIMALS_LTC_CNH, 2).
-define(RATE_DECIMALS_LTC_GBP, 3).
-define(RATE_DECIMALS_NMC_BTC, 5).
-define(RATE_DECIMALS_NMC_USD, 3).
-define(RATE_DECIMALS_NVC_BTC, 5).
-define(RATE_DECIMALS_NVC_USD, 3).
-define(RATE_DECIMALS_USD_RUR, 5).
-define(RATE_DECIMALS_EUR_USD, 5).
-define(RATE_DECIMALS_EUR_RUR, 5).
-define(RATE_DECIMALS_USD_CNH, 4).
-define(RATE_DECIMALS_GBP_USD, 4).
-define(RATE_DECIMALS_TRC_BTC, 5).
-define(RATE_DECIMALS_PPC_BTC, 5).
-define(RATE_DECIMALS_PPC_USD, 3).
-define(RATE_DECIMALS_FTC_BTC, 5).
-define(RATE_DECIMALS_XPM_BTC, 5).

%% max decimal places for amounts
-define(AMOUNT_DECIMALS_BTC_USD, 8).
-define(AMOUNT_DECIMALS_BTC_RUR, 8).
-define(AMOUNT_DECIMALS_BTC_EUR, 8).
-define(AMOUNT_DECIMALS_BTC_CNH, 8).
-define(AMOUNT_DECIMALS_BTC_GBP, 8).
-define(AMOUNT_DECIMALS_LTC_BTC, 8).
-define(AMOUNT_DECIMALS_LTC_USD, 8).
-define(AMOUNT_DECIMALS_LTC_RUR, 8).
-define(AMOUNT_DECIMALS_LTC_EUR, 8).
-define(AMOUNT_DECIMALS_LTC_CNH, 8).
-define(AMOUNT_DECIMALS_LTC_GBP, 8).
-define(AMOUNT_DECIMALS_NMC_BTC, 8).
-define(AMOUNT_DECIMALS_NMC_USD, 8).
-define(AMOUNT_DECIMALS_NVC_BTC, 8).
-define(AMOUNT_DECIMALS_NVC_USD, 8).
-define(AMOUNT_DECIMALS_USD_RUR, 8).
-define(AMOUNT_DECIMALS_EUR_USD, 8).
-define(AMOUNT_DECIMALS_EUR_RUR, 8).
-define(AMOUNT_DECIMALS_USD_CNH, 8).
-define(AMOUNT_DECIMALS_GBP_USD, 8).
-define(AMOUNT_DECIMALS_TRC_BTC, 8).
-define(AMOUNT_DECIMALS_PPC_BTC, 8).
-define(AMOUNT_DECIMALS_PPC_USD, 8).
-define(AMOUNT_DECIMALS_FTC_BTC, 8).
-define(AMOUNT_DECIMALS_XPM_BTC, 8).


%% used when post'ing data
-define(CONTENT_TYPE, "application/x-www-form-urlencoded").
