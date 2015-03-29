-record(f_node,
{
  pid       :: pid(),
  callback  :: fun()
}).

-record(network,
{
  graph :: atom()
}).
-type network() :: #network{}.
-type f_node()  :: #f_node{}.