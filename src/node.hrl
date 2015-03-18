-record(f_node,
{
  pid       :: pid(),
  callback  :: fun(),
  state     :: any()
}).

-record(network,
{
  entry :: node(),
  graph :: atom()
}).

-type network() :: #network{}.
-type f_node()  :: #f_node{}.