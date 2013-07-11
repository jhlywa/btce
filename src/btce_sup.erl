-module(btce_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Processes = [
                 {btce_key_server, {btce_key_server, start_link, []},
                  permanent, 1000, worker, [btce_key_server]}
                ],
    {ok, {{one_for_one, 5, 10}, Processes}}.
