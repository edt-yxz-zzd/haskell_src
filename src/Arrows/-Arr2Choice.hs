


type G arr i o = forall env t. arr (env, Either t i) (env, Either t o)
newtype Arr2Choice arr i o =
    Arr2Choice { runArr2Choice :: arr }
