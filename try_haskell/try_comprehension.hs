
import Control.Monad (guard)
data D = D0 | D1 Int

ds1 = [D0, D1 0, D1 2]
ds2 = [D1 1, D0, D0]
mds = [Just ds1, Just ds2, Nothing]

ls = [i | Just ds <- mds, D1 i <- ds]
-- ls == [0, 2, 1] ??? yes


-- ERROR: ls' = [i | Just ds <- mds, guard (null ds), D1 i <- ds]
ls' = [i | Just ds <- mds, (null ds), D1 i <- ds]
-- ls' == [] ??? yes
