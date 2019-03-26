
{-
 - each line : <indent> <node> ('/' <color>)? ('~' <weight>)? <succs>?
 - succs : <edge_t> (<node>* | <newline> <subblock>)
 - edge_t : ':' - default; '<' - backward; '>' - forward; '-' - undirected;
 - comment : regex'#.*'
 - indent : 4 ascii spaces
-}

