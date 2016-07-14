-module(edora_server_test).
-author("ferigis").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([join/1]).
-export([leave/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  edora:start(),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join(ClusterName) ->
  gen_server:call(?MODULE, {join, ClusterName}).

leave(ClusterName) ->
  gen_server:call(?MODULE, {leave, ClusterName}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #{}}.


handle_call({join, ClusterName}, _From, State) ->
  edora:join(ClusterName),
  {reply, ok, State};
handle_call({leave, ClusterName}, _From, State) ->
  edora:leave(ClusterName),
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #{}) ->
  {noreply, NewState :: #{}} |
  {noreply, NewState :: #{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #{}) ->
  {noreply, NewState :: #{}} |
  {noreply, NewState :: #{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #{}}).
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #{},
    Extra :: term()) ->
  {ok, NewState :: #{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
