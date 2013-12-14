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

-module(btce_key_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_key/3, add_key/4,
         del_key/1,
         get_key/0, get_key/1,
         set_default_key/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_key(ApiKey, PublicKey, PrivateKey) ->
    add_key(ApiKey, PublicKey, PrivateKey, []).

add_key(ApiKey, PublicKey, PrivateKey, Opts) when ApiKey =/= undefined ->
    gen_server:call(?MODULE, {add_key, ApiKey, PublicKey, PrivateKey, Opts}).

del_key(ApiKey) ->
    gen_server:call(?MODULE, {del_key, ApiKey}).

get_key() ->
    gen_server:call(?MODULE, get_key).

get_key(undefined) ->
    gen_server:call(?MODULE, get_key);
get_key(ApiKey) ->
    gen_server:call(?MODULE, {get_key, ApiKey}).

set_default_key(ApiKey) ->
    gen_server:call(?MODULE, {set_default_key, ApiKey}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}, 0}.

handle_call({add_key, ApiKey, PublicKey, PrivateKey, Opts}, _From, State) ->
    Nonce = proplists:get_value(nonce, Opts, 0),
    Default = proplists:get_value(default, Opts, false),

    Entries = case Default of
        true  -> [{ApiKey, PublicKey, PrivateKey, Nonce}, {default, ApiKey}];
        false -> [{ApiKey, PublicKey, PrivateKey, Nonce}]
    end,

    ok = dets:insert(?MODULE, Entries),
    {reply, ok, State};
handle_call({del_key, ApiKey}, _From, State) ->
    ok = dets:delete_object(?MODULE, {default, ApiKey}),
    ok = dets:delete(?MODULE, ApiKey),
    {reply, ok, State};
handle_call(get_key, _From, State) ->
    Result = case dets:lookup(?MODULE, default) of
        [] -> error;
        [{default, ApiKey}] -> inc_nonce(ApiKey)
    end,
    {reply, Result, State};
handle_call({get_key, ApiKey}, _From, State) ->
    {reply, inc_nonce(ApiKey), State};
handle_call({set_default_key, ApiKey}, _From, State) ->
    Result = case dets:lookup(?MODULE, ApiKey) of 
        [] -> {error, invalid_key};
        _  -> dets:insert(?MODULE, {default, ApiKey})
    end,
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    ok = filelib:ensure_dir("priv/"),
    case dets:open_file(?MODULE, [{file, "priv/btce_keys.dets"}]) of
        {ok, ?MODULE}   -> {noreply, State};
        {error, Reason} -> {stop, {shutdown, Reason}, State}
    end.

terminate(_Reason, _State) ->
    dets:close(?MODULE),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
inc_nonce(ApiKey) ->
    dets:update_counter(?MODULE, ApiKey, {4, 1}),
    [Result] = dets:lookup(?MODULE, ApiKey),
    Result.

