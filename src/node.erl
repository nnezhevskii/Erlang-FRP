-module(node).

-include("node.hrl").

-export([start_node/1,
  create_node/2,
  send_event/2, timer_loop/3, timer/3]).

-export([loop/1]).

-export_type([node/0]).


%% If message doesn't contain system information it's processed by a callback function UserHandler.
%% This message processed through pattern-matching.
%% Callback function can have side-effects
%% Recalculated result is sent to all listeners.
%% If node should not handle the message, it must be skipped through identical case of pattern-matching.

%% API

-spec start_node(fun()) -> {ok, node()}.
start_node(UserHandler) ->
  Node = create_node(UserHandler, self()),
  Pid = spawn_link(?MODULE, loop, [Node]),
  NewNode = update_node(Node, UserHandler, Pid),
  {ok, NewNode}.

send_event(Node, {event, Network, Msg}) ->
  Node#node.pid ! {event, Network, Msg},
  ok.

%% Other functions

-spec create_node(UserHandler, Pid) -> node() when
      UserHandler :: fun(),
      Pid         :: pid().

create_node(UserHandler, Pid) ->
  Node = #node{
    pid = Pid,
    callback  = UserHandler
  },
  Node.

-spec loop(node()) -> ok.
loop(Node) ->
  UserHandler = Node#node.callback,
  NNode = Node#node{callback = UserHandler, pid = self()},
  receive
    {update_node, NewNode} ->
      loop(NewNode);
    {event, Network, stop} ->
      Listeners = digraph:out_neighbours(Network#network.graph, NNode),
      [Child#node.pid ! {event, Network, stop} || Child <- Listeners],
      ok;
    {event, Network, Msg} ->
      Value = UserHandler(Msg),
      Listeners = digraph:out_neighbours(Network#network.graph, NNode),
      [Child#node.pid ! {event, Network, Value} || Child <- Listeners],
      loop(NNode)
  end.

update_node(HostNode, UserHandler, Pid) ->
  NewNode = create_node(UserHandler, Pid),
  HostNode#node.pid ! {update_node, NewNode},
  NewNode.

timer({Network, Node}, Msg, Interval) ->
  spawn_link(?MODULE, timer_loop, [Node, {event, Network, Msg}, Interval]).

timer_loop(Node, {event, Network, Msg}, Interval) ->
  node:send_event(Node, {event, Network, Msg}),
  timer:sleep(Interval),
  timer_loop(Node, {event, Network, Msg}, Interval).