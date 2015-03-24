-record(f_node,
{
  pid       :: pid(),
  callback  :: fun(),
  state     :: any()
}).

-record(network,
{
  entry :: dict:dict(),
  graph :: atom()
}).
-type network() :: #network{}.
-type f_node()  :: #f_node{}.