ArrSS (ArrCpsDetectT e r arr)
ArrSS (ArrCatchT e arr)

assume (IArrowReset arr, ArrowChoice arr)
    ==>> ArrowChoice/OpDetectSuccessA/OpDoOrNopA (ArrCatchT e arr)
    ==>> IArrowBiasedPlusSS/IArrowBiasedPlus (ArrSS (ArrCatchT e arr))
    here, IArrowBiasedPlusSS is suitable for parser

ArrCpsDetectT - for callCC
    instance (Arrow arr) => IArrowCC (ArrCpsDetectT e r arr)
    instance (Arrow arr, ArrowChoice arr) => IArrowCatch (ArrCpsDetectT e r arr)
    instance (OpLookAheadA arr, ArrowZero arr, ArrowChoice arr)
        => OpLookAheadA (ArrCpsDetectT e r arr)
