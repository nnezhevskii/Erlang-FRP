-module(node).

-include("node.hrl").

-export([
         start_node/0,
         start_node/1,
         start_node/2]).

-export([
         timer/3,
         send_event/2]).

-export([
         loop/2,
         timer_loop/3]).

%%
%% Creating nodes
%%

%% start_node/0 means callback function doesn't change state and value
-spec start_node() -> {ok, f_node()}.
start_node() ->
  F = fun(_State, _Value) -> {_State, _Value} end,
  start_node(F).

%% start_node/1 means node have custom callback function and don't have starting state
-spec start_node(fun()) -> {ok, f_node()}.
start_node(UserHandler) ->
  start_node(UserHandler, no_state).

%% start_node/2 defines custom callback function and starting state
-spec start_node(fun(), any()) -> {ok, f_node()}.
start_node(UserHandler, StartState) ->
  Node = create_node(UserHandler, self()),
  Pid = spawn_link(?MODULE, loop, [Node, StartState]),
  NewNode = update_node(Node, UserHandler, Pid),
  {ok, NewNode}.

%%
%% Control functions
%%

%% send message to node
-spec send_event(f_node(), {event, network(), term(), any()}) -> ok.
send_event(Node, {event, Network, NodeName, Msg}) ->
  Node#f_node.pid ! {event, Network, NodeName, Msg},
  ok.

%% every Interval(milliseconds) NodeName (Network) will receive message Msg
-spec timer({network(), term()}, any(), non_neg_integer()) -> no_return().
timer({Network, NodeName}, Msg, Interval) ->
  spawn_link(?MODULE, timer_loop, [NodeName, {event, Network, Msg}, Interval]).

%%
%% Other used functions
%%

%% construct f_node
-spec create_node(fun(), pid()) -> f_node().
create_node(UserHandler, Pid) ->
Node = #f_node{
  pid       = Pid,
  callback  = UserHandler
},
Node.

%% Body of process
-spec loop(f_node(), any()) -> no_return().
loop(Node, State) ->
  UserHandler = Node#f_node.callback,
  receive
    {update_node, NewNode} ->
      loop(NewNode, State);

    {event, Network, NodeName, stop} ->
      NNode = Node#f_node{callback = UserHandler, pid = self()},
      Listeners = digraph:out_neighbours(Network#network.graph, NNode),
      [Child#f_node.pid ! {event, Network, NodeName, stop} || Child <- Listeners],
      ok;

    {event, Network, NodeName, Msg} ->
      {NewState, Value} = UserHandler(State, Msg),
      NNode = #f_node{pid = self(), callback = Node#f_node.callback},
      {ok, FNodes} = network:get_listeners(Network, NodeName),
      [network:send_event(Network, FNodeName, Value) || FNodeName <- FNodes],
      loop(NNode, NewState)
  end.

-spec update_node(f_node(), fun(), pid()) -> f_node().
update_node(HostNode, UserHandler, Pid) ->
  NewNode = create_node(UserHandler, Pid),
  HostNode#f_node.pid ! {update_node, NewNode},
  NewNode.

%% Body of process used by timer
-spec timer_loop(term(), {term(), network(), any()}, non_neg_integer()) -> no_return().
timer_loop(NodeName, {event, Network, Msg}, Interval) ->
  {ok, Node} = network:get_fnode(Network, NodeName),
  node:send_event(Node, {event, Network, Msg}),
  timer:sleep(Interval),
  timer_loop(NodeName, {event, Network, Msg}, Interval).