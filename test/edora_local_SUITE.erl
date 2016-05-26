-module(edora_local_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common test
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests
-export([create_vcluster/1]).
-export([join_vcluster/1]).
-export([leave_vcluster/1]).
-export([delete_vcluster/1]).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() ->
  [create_vcluster,
    join_vcluster,
    leave_vcluster,
    delete_vcluster].

init_per_suite(Config) ->
  Config.

end_per_suite(Config) ->
  Config.

%%%===================================================================
%%% Exported Tests Functions
%%%===================================================================

create_vcluster(_Config) ->
  ClusterName = vcluster,
  EdoraClusterName = edora:vcluster_name(ClusterName),
  false = lists:member(EdoraClusterName, pg2:which_groups()),
  ok = edora:join(ClusterName),
  true = lists:member(EdoraClusterName, pg2:which_groups()),
  [Pid] = pg2:get_members(EdoraClusterName),
  Pid = self(),
  already_joined = edora:join(ClusterName).

join_vcluster(_Config) ->
  ClusterName = vcluster2,
  no_virtual_cluster = edora:vcluster(ClusterName),
  ok = edora:join(ClusterName),
  [Node] = edora:vcluster(ClusterName),
  Node = node().

leave_vcluster(_Config) ->
  ClusterName = vcluster3,
  no_virtual_cluster = edora:leave(ClusterName),
  ok = edora:join(ClusterName),
  [Node] = edora:vcluster(ClusterName),
  Node = node(),
  ok = edora:leave(ClusterName),
  [] = edora:vcluster(ClusterName),
  ok = edora:join(ClusterName),
  already_joined = edora:join(ClusterName),
  [Node] = edora:vcluster(ClusterName),
  ok = edora:leave(ClusterName),
  [] = edora:vcluster(ClusterName).

delete_vcluster(_Config) ->
  ClusterName = vcluster4,
  no_virtual_cluster = edora:leave(ClusterName),
  ok = edora:join(ClusterName),
  [Node] = edora:vcluster(ClusterName),
  Node = node(),
  ok = edora:leave(ClusterName),
  [] = edora:vcluster(ClusterName),
  ok = edora:join(ClusterName),
  already_joined = edora:join(ClusterName),
  [Node] = edora:vcluster(ClusterName),
  ok = edora:delete(ClusterName),
  no_virtual_cluster = edora:vcluster(ClusterName).