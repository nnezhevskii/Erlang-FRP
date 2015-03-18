-module(network).

-include("node.hrl").

-export([create_network/1,
         add_listener/3,
         make_mailing/2,
         add_listeners/3,
         stop/1]).

-spec create_network(node()) -> network().
create_network(Node) ->
  NetworkGraph = digraph:new(),
  digraph:add_vertex(NetworkGraph, Node),
  Network = #network{entry = Node, graph = NetworkGraph},
  Network.

-spec add_listener(network(), f_node(), f_node()) -> {ok, network()}.
add_listener(Network, Host, Node) ->
  Graph = Network#network.graph,
  digraph:add_vertex(Graph, Node),
  case digraph:vertex(Graph, Node) of
    false ->
      digraph:add_vertex(Graph, Node);
    _ ->
      ok
  end,
  digraph:add_edge(Graph, Host, Node),
  NewNetwork = Network#network{entry = Network#network.entry, graph = Graph},
  {ok, NewNetwork}.


make_mailing(Network, Msg) ->
  make_mailing(Network, Network#network.entry, Msg).

make_mailing(Network, Node, Msg) ->
  node:send_event(Node,{event, Network, Msg}),
  ok.

add_listeners(Network, Host, Nodes) when is_list(Nodes) ->
  [network:add_listener(Network, Host, Node) || Node <- Nodes],
  ok.

stop(Network) ->
  node:send_event(Network#network.entry, {event, Network, stop}).
