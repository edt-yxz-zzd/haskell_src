

class (Container a, Boolean (NullExResult a)) => OpNull a where
    null :: a -> Bool
    null = explain . null_ex
    type NullExResult a :: *
    null_ex :: a -> NullExResult a
    default null_ex :: OpDynTheOnlyValue (NullExResult a)
        => a -> NullExResult a
    null_ex _ = the_only_value

