-module(edora_dist_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common test
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests
-export([join_vcluster/1]).
-export([leave_vcluster/1]).
-export([delete_vcluster/1]).

-define(NODE_NAME_1, edora_node1).
-define(NODE_NAME_2, edora_node2).
-define(NODE_NAME_3, edora_node3).
-define(NODE_NAME_4, edora_node4).
-define(NODE_NAME_5, edora_node5).
-define(NODES, [ ?NODE_NAME_1
                , ?NODE_NAME_2
                , ?NODE_NAME_3
                , ?NODE_NAME_4
                , ?NODE_NAME_5]).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() ->
  [join_vcluster
    , leave_vcluster
    , delete_vcluster
  ].

init_per_suite(Config) ->
  edora:start(),
  Nodes = [Node || {ok, Node} <- [start_node(NodeShortName) || NodeShortName <- ?NODES]],
  [net_kernel:connect_node(Node) || Node <- Nodes],
  Config.

end_per_suite(Config) ->
  edora:stop(),
  [stop_node(NodeShortName) || NodeShortName <- ?NODES],
  Config.

%%%===================================================================
%%% Exported Tests Functions
%%%===================================================================

join_vcluster(_Config) ->
  ClusterName = dist_vcluster,
  no_virtual_cluster = edora:vcluster(ClusterName),
  ok = edora:join(ClusterName),
  [Node] = edora:vcluster(ClusterName),
  Node = node(),
  [rpc:call(Node_, edora_server_test, join, [ClusterName]) || Node_ <- nodes()],
  Nodes = nodes() ++ [node()],
  Nodes = edora:vcluster(ClusterName).


leave_vcluster(_Config) ->
  ClusterName = dist_vcluster2,
  no_virtual_cluster = edora:vcluster(ClusterName),
  ok = edora:join(ClusterName),
  [Node] = edora:vcluster(ClusterName),
  Node = node(),
  [rpc:call(Node_, edora_server_test, join, [ClusterName]) || Node_ <- nodes()],
  Nodes = nodes() ++ [node()],
  Nodes = edora:vcluster(ClusterName),
  [rpc:call(Node_, edora_server_test, leave, [ClusterName]) || Node_ <- nodes()],
  [Node] = edora:vcluster(ClusterName).

delete_vcluster(_Config) ->
  ClusterName = dist_vcluster3,
  no_virtual_cluster = edora:vcluster(ClusterName),
  ok = edora:join(ClusterName),
  [Node] = edora:vcluster(ClusterName),
  Node = node(),
  [rpc:call(Node_, edora_server_test, join, [ClusterName]) || Node_ <- nodes()],
  Nodes = nodes() ++ [node()],
  Nodes = edora:vcluster(ClusterName),
  ok = edora:delete(ClusterName),
  no_virtual_cluster = edora:vcluster(ClusterName).

%% Internal Functions

start_node(NodeName) ->
  CodePath = code:get_path(),
  {ok, Node} = ct_slave:start(NodeName, [{monitor_master, true}]),
  true = rpc:call(Node, code, set_path, [CodePath]),
  rpc:call(Node, edora_server_test, start_link, []),
  {ok, Node}.

stop_node(NodeShortName) ->
  {ok, _} = ct_slave:stop(NodeShortName).