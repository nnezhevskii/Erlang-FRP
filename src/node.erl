-module(node).
-export([start_node/2,
         add_listener/2,
         terminate/0,
         timer/3,
         timer_loop/3,
         create_node/3,
         send_event/2]).

-export([loop/1]).

-export_type([node/0]).

-record(node,
    {
      pid       :: pid(),
      listeners :: list(node()),
      callback  :: fun()
    }).

%% If message doesn't contain system information it's processed by a callback function UserHandler.
%% This message processed through pattern-matching.
%% Callback function can have side-effects
%% Recalculated result is sent to all listeners.
%% If node should not handle the message, it must be skipped through identical case of pattern-matching.

%% API

-spec start_node(list(pid()), fun()) -> {ok, node()}.
start_node(Listeners, UserHandler) ->
  Node = create_node(Listeners, UserHandler, self()),
  Pid = spawn_link(?MODULE, loop, [Node]),
  NewNode = update_node(Node, Listeners, UserHandler, Pid),
  {ok, NewNode}.

add_listener(Host, Listener) ->
  Host#node.pid ! {new_listener, Listener}.

timer(Node, Msg, Interval) ->
  spawn(?MODULE, timer_loop, [Node#node.pid, Msg, Interval]).

send_event(Node, Msg) ->
  Node#node.pid ! Msg.

terminate() ->
  self ! stop.


%% Other functions

create_node(Listeners, UserHandler, Pid) ->
  Node = #node{
            pid = Pid,
            listeners = Listeners,
            callback  = UserHandler
        },
  Node.

-spec loop(node()) -> ok.
loop(Node) ->
  Listeners   = Node#node.listeners,
  UserHandler = Node#node.callback,
  receive
    stop ->
      send_all(Listeners, {system_command, stop}),
      ok;
    {new_listener, NNode} ->
      NewNode = create_node(Listeners ++ [NNode], UserHandler, self()),
      loop(NewNode);
    {update_node, NNode} ->
      loop(NNode);
    Msg ->
      send_all(Listeners, UserHandler(Msg)),
      loop(Node)
  end.


update_node(HostNode, Listeners, UserHandler, Pid) ->
  NewNode = create_node(Listeners, UserHandler, Pid),
  HostNode#node.pid ! {update_node, NewNode},
  NewNode.

-spec send_all(list(pid()), term()) -> ok.
send_all(Nodes, Msg) ->
  [Node#node.pid ! Msg ||  Node <- Nodes],
  ok.

timer_loop(Pid, Msg, Interval) ->
  Pid ! Msg,
  timer:sleep(Interval),
  timer_loop(Pid, Msg, Interval).