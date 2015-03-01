-module(main).

%% API
-export([main/0,
         print_value_fst/1,
         print_value_snd/1]).

print_value_fst(X) ->
  io:format("~s I'm first process~n", [X]),
  X ++ "(modified1) ".

print_value_snd(X) ->
  io:format("~s I'm second process~n", [X]),
  X ++ "(modified2) ".

main() ->
  {ok, Node} = node:start_node([], fun(X) -> main:print_value_fst(X) end),
  node:timer(Node, "Hello, world!", 1000).
%%   {ok, SndNode} = node:start_node([], fun(X) -> main:print_value_snd(X) end),
%%   node:send_event(Node, "Hey, cruel world!"),
%%   timer:sleep(1000),
%%   io:format("~n~n~n"),
%%   node:add_listener(Node, SndNode),
%%   node:send_event(Node, "Hey, cruel world!").
%%   {ok, Pid2} = node:start_node([], fun(X) -> main:print_value_snd(X) end),
%%   node:add_listener(Pid1, Pid2),
%%   node:timer(Pid1, "Hello! Hello! Hello!", 5000).