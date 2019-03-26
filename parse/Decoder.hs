

{-
    D - decode/decrypt
    E - encode/encrypt
    n - name
-}

class Codec n where
    name :: n -> String
    type D n
    type E n

class Codec n => Encoder n where
    encode :: n -> D n -> E n

data G_DecodeResult n
    = Clear (D n)
    | Error
    | Free  -- undefined
            -- NotImplemented
            -- nondeterministic ==>> can be any (D n) or Error
            --      e.g. when two super decoders(finer) don't agree
            --           i.e. Free != decode n1 e != decode n2 e != Free
            -- unknown
class (Codec n, To (G_DecodeResult n) (DecodeResult n)
    => Decoder n where
    type DecodeResult n
    decode :: n -> E n -> DecodeResult n

{-
    clears n = {e | decode n e = Clear _}
    errors n = {e | decode n e = Error}
    frees n = {e | decode n e = Free}
    decoder relationship
        # inverse   coarser/sub/general
        finer/refinement/super/special super sub ::=
            # transitive, reflex
            E super == E sup
            clears sub <= clears super
            errors sub <= errors super
            # frees sub >= frees super
            for e in clears sub:
                decode super e == decode sub e
            clears sub  ----id---->  clears super
                        /--------/
            frees  sub <-----------  frees  super
                        \--------\
            errors sub  ---------->  errors super
        agree n1 n2 ::=
            # not transitive
            # reflex
            E n1 == E n2
            forall e:
                let d1 = decode n1 e
                    d2 = decode n2 e
                d1 == Free || d2 == Free || d1 == d2
            clears sub <----id----->  clears super
                        >---------<
            frees  sub <----------->  frees  super
                        >---------<
            errors sub <----------->  errors super


-}
