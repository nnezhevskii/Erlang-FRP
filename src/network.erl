-module(network).

-include("node.hrl").

-export([create_network/1,
         add_listener/3,
         make_mailing/2]).

-export([make_mailing/3]).

create_network(Node) ->
  NetworkGraph = digraph:new(),
  digraph:add_vertex(NetworkGraph, Node),
  #network{entry = Node, graph = NetworkGraph}.

add_listener(Network, Host, Node) ->
  Graph = Network#network.graph,
  digraph:add_vertex(Graph, Node),
  digraph:add_edge(Graph, Host, Node),
  NewNetwork = Network#network{entry = Network#network.entry, graph = Graph},
  {ok, NewNetwork}.

make_mailing(Network, Msg) ->
  make_mailing(Network, Network#network.entry, Msg).

make_mailing(Network, Node, Msg) ->
  node:send_event(Node,{event, Network, Msg}),

  ok.