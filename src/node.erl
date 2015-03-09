-module(node).

-include("node.hrl").

-export([start_node/1,
  create_node/2,
  send_event/2, timer_loop/3, timer/3]).

-export([loop/1]).


%% If message doesn't contain system information it's processed by a callback function UserHandler.
%% This message processed through pattern-matching.
%% Callback function can have side-effects
%% Recalculated result is sent to all listeners.
%% If node should not handle the message, it must be skipped through identical case of pattern-matching.

%% API

-spec start_node(fun()) -> {ok, f_node()}.
start_node(UserHandler) ->
  Node = create_node(UserHandler, self()),
  Pid = spawn_link(?MODULE, loop, [Node]),
  NewNode = update_node(Node, UserHandler, Pid),
  {ok, NewNode}.

send_event(Node, {event, Network, Msg}) ->
  Node#f_node.pid ! {event, Network, Msg},
  ok.

%% Other functions

-spec create_node(UserHandler, Pid) -> f_node() when
      UserHandler :: fun(),
      Pid         :: pid().

create_node(UserHandler, Pid) ->
  Node = #f_node{
    pid = Pid,
    callback  = UserHandler
  },
  Node.

-spec loop(f_node()) -> no_return().
loop(Node) ->
  UserHandler = Node#f_node.callback,
  NNode = Node#f_node{callback = UserHandler, pid = self()},
  receive
    {update_node, NewNode} ->
      loop(NewNode);
    {event, Network, stop} ->
      Listeners = digraph:out_neighbours(Network#network.graph, NNode),
      [Child#f_node.pid ! {event, Network, stop} || Child <- Listeners],
      ok;
    {event, Network, Msg} ->
      Value = UserHandler(Msg),
      Listeners = digraph:out_neighbours(Network#network.graph, NNode),
      [Child#f_node.pid ! {event, Network, Value} || Child <- Listeners],
      loop(NNode)
  end.

-spec update_node(f_node(), fun(), pid()) -> f_node().
update_node(HostNode, UserHandler, Pid) ->
  NewNode = create_node(UserHandler, Pid),
  HostNode#f_node.pid ! {update_node, NewNode},
  NewNode.

-spec timer({network(), f_node()}, any(), non_neg_integer()) -> no_return().
timer({Network, Node}, Msg, Interval) ->
  spawn_link(?MODULE, timer_loop, [Node, {event, Network, Msg}, Interval]).

-spec timer_loop(f_node(), {term(), network(), any()}, non_neg_integer()) -> no_return().
timer_loop(Node, {event, Network, Msg}, Interval) ->
  node:send_event(Node, {event, Network, Msg}),
  timer:sleep(Interval),
  timer_loop(Node, {event, Network, Msg}, Interval).