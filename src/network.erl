-module(network).

-include("node.hrl").

-export([
         create_network/0,
         add_listener/3,
         send_event/3,
         add_listeners/3,
         stop/1,
         add_node/2,
         update_node/2,
         get_fnode/2,
         get_listeners/2,
         add_nodes/2
        ]).

%% Constructing network. This network has one node named external_entry
%% external_entry can be used as source of flow.

-spec create_network() -> network().
create_network() ->
  NetworkGraph = new_network_graph(),
  {ok, Entry} = node:start_node(
    fun(_X, _State) ->
      {_State, _X}
    end, no_state),
  Network = #network{graph = NetworkGraph},
  {ok, external_entry} = add_node(Network, {external_entry, Entry}),
  Network.

%% Addition new node in network.
%% This node must have unique name.

-spec add_node(network(), {term(), f_node()}) -> {ok, term()} | {error, existing_node}.
add_node(Network, {NodeName, Node}) ->
  NetGraph = Network#network.graph,
  case digraph:vertex(NetGraph, NodeName) of
    false ->
      digraph:add_vertex(NetGraph, NodeName, Node),
      {ok, NodeName};
    _ ->
      {error, existing_node}
  end.

-spec add_nodes(network(), list(term())) -> list().
add_nodes(Network, Nodes) ->
  [add_node(Network, {NodeName, Node}) || {NodeName, Node} <- Nodes].

%% Setting dependency between pair of node.

-spec add_listener(network(), term(), term()) -> {ok, network()}.
add_listener(Network, Host, Node) ->
  case {get_fnode(Network, Host), get_fnode(Network, Node)} of
    {{ok, _}, {ok, _}} ->
      digraph:add_edge(Network#network.graph, Host, Node),
      {ok, Host, Node};
    _ ->
      error
  end.

-spec add_listeners(network(), f_node(), list(f_node())) -> ok.
add_listeners(Network, Host, Nodes) when is_list(Nodes) ->
  [network:add_listener(Network, Host, Node) || Node <- Nodes].

%% Sending event to node in network.
%% Network has to have this node

-spec send_event(network(), term(), any()) -> ok.
send_event(Network, NodeName, Msg) ->
  case get_fnode(Network, NodeName) of
    {ok, FNode} ->
      node:send_event(FNode, {event, Network, NodeName,  Msg});
    Msg ->
      Msg
  end.

%%
%% Other functions
%%

-spec new_network_graph() -> digraph:digraph().
new_network_graph() ->
  digraph:new().


-spec update_node(network(), {term(), f_node()}) -> {ok, term()} | {error, unknown_node}.
update_node(Network, {NodeName, Node}) ->
  NetGraph = Network#network.graph,
  case digraph:vertex(NetGraph, NodeName) of
    false ->
      {error, unknown_node};
    {NodeName, _} ->
      digraph:add_vertex(NetGraph, NodeName, Node),
      {ok, NodeName}
  end.

-spec get_fnode(network(), term()) -> {ok, f_node()} | {error, unknown_node}.
get_fnode(Network, NodeName) ->
  NetGraph = Network#network.graph,
  case digraph:vertex(NetGraph, NodeName) of
    false ->
      {error, unknown_node};
    {_, FNode} ->
      {ok, FNode}
  end.

-spec get_listeners(network(), term()) -> {ok, list(f_node())} | {error, unknown_node}.
get_listeners(Network, NodeName) ->
  NetGraph = Network#network.graph,
  case digraph:vertex(NetGraph, NodeName) of
    false ->
      {error, unknown_node};
    _ ->
      Listeners = digraph:out_neighbours(NetGraph, NodeName),
      {ok, Listeners}
  end.

-spec stop(network()) -> no_return().
stop(Network) ->
  node:send_event(Network#network.entry, {event, Network, stop}).