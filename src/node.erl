-module(node).

-include("node.hrl").

-export([start_node/1,
         start_node/2,
         create_node/2,
         send_event/2,
         timer/3]).

-export([loop/2,
         timer_loop/3]).

-spec start_node(fun(), any()) -> {ok, f_node()}.
start_node(UserHandler, StartState) ->
  Node = create_node(UserHandler, self()),
  Pid = spawn_link(?MODULE, loop, [Node, StartState]),
  NewNode = update_node(Node, UserHandler, Pid),
  {ok, NewNode}.

-spec start_node(fun()) -> {ok, f_node()}.
start_node(UserHandler) ->
  start_node(UserHandler, no_state).

-spec create_node(fun(), pid()) -> f_node().
create_node(UserHandler, Pid) ->
  Node = #f_node{
    pid       = Pid,
    callback  = UserHandler
  },
  Node.

-spec send_event(f_node(), {event, network(), term(), any()}) -> ok.
send_event(Node, {event, Network, NodeName, Msg}) ->
  Node#f_node.pid ! {event, Network, NodeName, Msg},
  ok.

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

-spec timer({network(), f_node()}, any(), non_neg_integer()) -> no_return().
timer({Network, NodeName}, Msg, Interval) ->
  spawn_link(?MODULE, timer_loop, [NodeName, {event, Network, Msg}, Interval]).

-spec timer_loop(term(), {term(), network(), any()}, non_neg_integer()) -> no_return().
timer_loop(NodeName, {event, Network, Msg}, Interval) ->
  {ok, Node} = network:get_fnode(Network, NodeName),
  node:send_event(Node, {event, Network, Msg}),
  timer:sleep(Interval),
  timer_loop(NodeName, {event, Network, Msg}, Interval).