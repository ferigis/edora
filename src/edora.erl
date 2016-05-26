%%%-------------------------------------------------------------------
%%% @doc
%%% This is the main module, which contains all API functions.
%%% @end
%%%-------------------------------------------------------l------------
-module(edora).

%% API
-export([join/1]).
-export([leave/1]).
-export([delete/1]).
-export([vcluster/1]).
-export([vcluster_name/1]).

%% Types
-type cluster_name() :: atom().
-type virtual_cluster_name() :: atom().

%%%===================================================================
%%% edora API
%%%===================================================================

-spec join(cluster_name()) -> already_joined | ok.
join(Name) when is_atom(Name) ->
  global:trans({?MODULE, Name},
    fun() ->
      case already_joined(Name) of
        true  -> already_joined;
        false -> join_(Name)
      end
    end).

-spec leave(cluster_name()) -> no_virtual_cluster | ok.
leave(Name) ->
  global:trans({?MODULE, Name},
    fun() ->
      VClusterName = vcluster_name(Name),
      leave_from_pids(pg2:get_local_members(VClusterName), VClusterName)
    end).

-spec delete(cluster_name()) -> ok.
delete(Name) ->
  global:trans({?MODULE, Name},
    fun() ->
      VClusterName = vcluster_name(Name),
      pg2:delete(VClusterName)
    end).

-spec vcluster(cluster_name()) -> no_virtual_cluster | [node()].
vcluster(Name) ->
  global:trans({?MODULE, Name},
    fun() ->
      VClusterName = vcluster_name(Name),
      nodes_from_pids(pg2:get_members(VClusterName))
    end).

-spec vcluster_name(cluster_name()) -> virtual_cluster_name().
vcluster_name(Name) when is_atom(Name) ->
  Bin = <<(atom_to_binary(?MODULE, utf8))/binary, "_",
    (atom_to_binary(Name, utf8))/binary>>,
  binary_to_atom(Bin, utf8).


%% Internal functions

%% @private
-spec(join_(cluster_name()) -> ok).
join_(Name) ->
  VClusterName = vcluster_name(Name),
  case pg2:join(VClusterName, self()) of
    {error,{no_such_group, VClusterName}} ->
      pg2:create(VClusterName),
      pg2:join(VClusterName, self());
    ok -> ok
  end.

%% @private
-spec nodes_from_pids({error, {no_such_group, virtual_cluster_name()}}) -> no_virtual_cluster;
                     ([pid()]) -> [node()].
nodes_from_pids({error,{no_such_group, _VClusterName}}) -> no_virtual_cluster;
nodes_from_pids(Pids) ->
  Nodes = [node(Pid) || Pid <- Pids],
  sets:to_list(sets:from_list(Nodes)).

%% @private
-spec leave_from_pids({error, {no_such_group, virtual_cluster_name()}}
                      , virtual_cluster_name()) -> no_virtual_cluster;
                     ([pid()], virtual_cluster_name()) -> ok.
leave_from_pids({error,{no_such_group, _VClusterName}}, _VClusterName) -> no_virtual_cluster;
leave_from_pids(Pids, VClusterName) ->
  [pg2:leave(VClusterName, Pid) || Pid <- Pids],
  ok.

%% @private
-spec already_joined(cluster_name()) -> boolean().
already_joined(Name) ->
  VCluster = vcluster(Name),
  case VCluster of
    no_virtual_cluster -> false;
    VCluster -> lists:member(node(), VCluster)
  end.
