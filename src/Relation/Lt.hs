
-- partial ordering
class Lt a b

data Or a b
data And a b
data Atom a

instance (Lt a c, Lt b c) => Lt (Or a b) c
instance (Lt a c | Lt b c) => Lt (And a b) c





