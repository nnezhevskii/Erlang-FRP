-record(node,
{
  pid       :: pid(),
  callback  :: fun()
}).

-record(network,
{
  entry :: node(),
  graph :: atom()
}).

