-module(network).

-include("node.hrl").

-export([create_network/0,
  add_listener/3,
  send_event/3,
  add_listeners/3,
  stop/1, add_entry/2]).

-spec create_network() -> network().
create_network() ->
  NetworkGraph = digraph:new(),
  {ok, Entry} = node:start_node(fun(_X, _State) ->
    {_State, _X}
  end, no_state),
  digraph:add_vertex(NetworkGraph, Entry),
  #network{graph = NetworkGraph, entry = dict:new()}.

-spec add_entry(network(), {term(), f_node()}) -> {ok, network()}.
add_entry(Network, {NameEntry, Entry}) ->
%%   TODO: checking...
  NewProcessDict = dict:append(NameEntry, Entry, Network#network.entry),
  digraph:add_vertex(Network#network.graph, Entry),
  NewNetwork = #network{entry = NewProcessDict, graph = Network#network.graph},
  {ok, NewNetwork}.

-spec send_event(network(), term(), any()) -> ok.
send_event(Network, Entry, Msg) ->
  %%   TODO: checking...
  {ok, [Node]} = dict:find(Entry, Network#network.entry),
  node:send_event(Node, {event, Network, Msg}),
  ok.


-spec add_listener(network(), f_node(), f_node()) -> {ok, network()}.
add_listener(Network, Host, Node) ->
  Graph = Network#network.graph,
  digraph:add_vertex(Graph, Node),
  digraph:add_edge(Graph, Host, Node),
  NewNetwork = Network#network{entry = Network#network.entry, graph = Graph},
  {ok, NewNetwork}.

add_listeners(Network, Host, Nodes) when is_list(Nodes) ->
  [network:add_listener(Network, Host, Node) || Node <- Nodes],
  ok.

stop(Network) ->
  node:send_event(Network#network.entry, {event, Network, stop}).
