-module(btce_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

-include("btce.hrl").

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),
    btce_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
