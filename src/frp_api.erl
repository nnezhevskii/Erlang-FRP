-module(frp_api).

-include("node.hrl").

-export([
         start_node/0,
         start_node/1,
         start_node/2
        ]).

-export([
         create_network/0,
         event/2,
         timer/3
        ]).

-export([add_node/2,
         add_nodes/2,
         add_listener/3,
         add_listeners/3
        ]).


-spec start_node() -> {ok, f_node()}.
start_node() ->
  node:start_node().

-spec start_node(fun()) -> {ok, f_node()}.
start_node(UserHandler) ->
  node:start_node(UserHandler, no_state).

-spec start_node(fun(), any()) -> {ok, f_node()}.
start_node(UserHandler, StartState) ->
  node:start_node(UserHandler, StartState).

-spec create_network() -> network().
create_network() ->
  network:create_network().

-spec event({network(), term()}, any()) -> ok | {error, unknown_node}.
event({Network, NodeName}, Msg) ->
  network:send_event(Network, NodeName, Msg).

-spec timer({network(), term()}, any(), non_neg_integer()) -> no_return().
timer({Network, NodeName}, Msg, Interval) ->
  node:timer({Network, NodeName}, Msg, Interval).

-spec add_node(network(), {term(), f_node()}) -> {ok, term()} | {error, existing_node}.
add_node(Network, {NodeName, Node}) ->
  network:add_node(Network, {NodeName, Node}).

-spec add_nodes(network(), list(term())) -> list().
add_nodes(Network, Nodes) ->
  network:add_nodes(Network, Nodes).

-spec add_listener(network(), term(), term()) -> {ok, network()}.
add_listener(Network, Host, Node) ->
  network:add_listener(Network, Host, Node).

-spec add_listeners(network(), f_node(), list(f_node())) -> list(term()).
add_listeners(Network, Host, Nodes) ->
  network:add_listeners(Network, Host, Nodes).

