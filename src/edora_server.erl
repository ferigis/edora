%%%-------------------------------------------------------------------
%%% @doc
%%% This module is a gen server. It is joined or not to pg2 group.
%%% @end
%%%-------------------------------------------------------------------
-module(edora_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([join/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @hidden
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

join(VClusterName) ->
  gen_server:call(?SERVER, {join, VClusterName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([]) ->
  {ok, #{}}.

handle_call({join, VClusterName}, _From, State) ->
  case pg2:join(VClusterName, self()) of
    {error,{no_such_group, VClusterName}} ->
      pg2:create(VClusterName),
      pg2:join(VClusterName, self());
    _ -> ok
  end,
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.