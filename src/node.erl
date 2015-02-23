-module(node).
-export([start_node/2,
         add_listener/1,
         terminate/0]).

-export([loop/2]).

%%
%% If message doesn't contain system information it's processed by a callback function UserHandler.
%% This message processed through pattern-matching.
%% Callback function can have side-effects
%% Recalculated result is sent to all listeners.
%%

start_node(Listeners, UserHandler) ->
  Pid = spawn_link(?MODULE, loop, [Listeners, UserHandler]),
  {ok, Pid}.

loop(Listeners, UserHandler) ->
  receive
    stop ->
      send_all(Listeners, {system_command, stop});
    {new_listener, Pid} ->
      loop(Listeners ++ [Pid], UserHandler);
    Msg ->
      send_all(Listeners, UserHandler(Msg))
  end.


send_all(Pids, Msg) ->
  [Pid ! Msg || Pid <- Pids].

add_listener(Pid) ->
  self() ! {new_listener, Pid}.

terminate() ->
  self ! stop.