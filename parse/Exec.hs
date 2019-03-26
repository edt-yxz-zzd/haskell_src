{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}



module Exec where
-- orignal name: "Formal"


import Container


conf_append_instructions
    :: (Exec st i sys, ins~Instructions sys, conf~(Instructions sys, st))
    => sys -> ins -> conf -> conf
conf_append_instructions _ ins (ins', st) = (ins' >< ins, st)
class   --( FiniteContainer i (Instructions sys)
        --, ListLike i (Instructions sys)
        --( [i] ~ Instructions sys -- cancel Finite requirement
        ( ListLike i (Instructions sys)
        , Set (Instructions sys, st) (Configurations sys)
        , OpMap (Instructions sys, st) (Configurations sys)
        , Iterable (Instructions sys, st) (Configurations sys)
        )
    => Exec st i sys | sys -> st i where
    -- st - state ; include (output, memory, input)
    -- i - instruction
    -- sys - executor, interpretor; system e.g. proof-system

    type Instructions sys :: *
    type Configurations sys :: *
    exec :: sys -> i -> st -> Configurations sys
    -- exec :: sys -> i -> st -> [([i], st)]
        --  exec on i:ls -> (ls', st) ==>> (ls'++ls, st)

    yield :: sys -> (Instructions sys, st) -> Configurations sys
    yield sys (ils, st) = case viewl ils of
        i :< ls -> omap (conf_append_instructions sys ls)$ exec sys i st
        EmptyL -> empty -- no instructions yields nothing



