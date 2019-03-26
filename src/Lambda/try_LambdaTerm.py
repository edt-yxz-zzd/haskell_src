

from LambdaTerm import *


BI = apps(B, I)
# (\x.\y.\z. x (y z)) (\x.x) = \y.\z. (\x.x) (y z) = \y.\z. y z = \y.y
BI = evalLambdaTerm(BI)
print('BI=', BI)
