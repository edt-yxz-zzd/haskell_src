
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


module StreamingAlgorithm
    (StreamingAlgorithm(..)
    ,streaming
    ,streaming_ex
    )
where

import TheType
import Standardizable

class Standardizable state => StreamingAlgorithm state where
    {-
    f :: State -> [may_infinite[input]] -> [Output]
    f state multiway_inputss = (f state) [II inputs] = II outputs
    try
        poll state directly to product one output
            # state contains interval of each remaining inputs way
    if fail:
        select one inputs way to consume one input
    -}
    {- # MINIMAL
            update_after_consume
            , (maybe_poll | maybe_peek, unsafe_update_after_poll)
        # -}

    type OutputType4StreamingAlgorithm state :: *
    type InputType4StreamingAlgorithm state :: *
    type MultiwayInputType4StreamingAlgorithm state :: *
    type InputWaySelectorType4StreamingAlgorithm state :: *

    maybe_poll_output
        :: Standard state
        -> Either   (InputWaySelectorType4StreamingAlgorithm state)
                    (OutputType4StreamingAlgorithm state, Standard state)
    get_inputs
        :: m state
        -> InputWaySelectorType4StreamingAlgorithm state
        -> MultiwayInputType4StreamingAlgorithm state
        -> [InputType4StreamingAlgorithm state]
    set_inputs
        :: m state
        -> InputWaySelectorType4StreamingAlgorithm state
        -> [InputType4StreamingAlgorithm state]
        -> MultiwayInputType4StreamingAlgorithm state
        -> MultiwayInputType4StreamingAlgorithm state

    update_after_consume
        :: Standard state
        -> InputWaySelectorType4StreamingAlgorithm state
        -> InputType4StreamingAlgorithm state
        -> Standard state

    maybe_consume_input
        :: m state
        -> InputWaySelectorType4StreamingAlgorithm state
        -> MultiwayInputType4StreamingAlgorithm state
        -> Maybe (InputType4StreamingAlgorithm state
                 ,MultiwayInputType4StreamingAlgorithm state)
    maybe_consume_input _ selector multiway_inputss
        = f inputs
        where
            stateT = TheType :: forall. TheType state
            inputs = get_inputs stateT selector multiway_inputss
            f [] = Nothing
            f (input:inputs') = Just (input, multiway_inputss')
                where
                    multiway_inputss' = set_inputs stateT selector inputs' multiway_inputss


streaming
    :: forall state. StreamingAlgorithm state
    => Standard state
    -> MultiwayInputType4StreamingAlgorithm state
    -> [OutputType4StreamingAlgorithm state]
streaming std_state inputss = do
    (may_output, _, _) <- streaming_ex std_state inputss
    maybe [] return may_output

-- not ouputput
type Private_Output3 state =
        (Maybe (OutputType4StreamingAlgorithm state)
        ,Standard state
        ,MultiwayInputType4StreamingAlgorithm state
        )

streaming_ex
    :: forall state. StreamingAlgorithm state
    => Standard state
    -> MultiwayInputType4StreamingAlgorithm state
    -> [(Maybe (OutputType4StreamingAlgorithm state)
        ,Standard state
        ,MultiwayInputType4StreamingAlgorithm state
        )]
streaming_ex = f . mkStandard where
    mkStandard = id
    unStandard = id
    f :: forall. Standard state
        -> MultiwayInputType4StreamingAlgorithm state
        -> [Private_Output3 state]
    f std_state inputss = case maybe_poll_output (unStandard std_state) of
        Right (output, nonstd_state)
            -> let std_state' = mkStandard nonstd_state
                in (Just output, std_state', inputss)
                    : f std_state' inputss
        Left selector -> g std_state selector inputss

    g :: forall. Standard state
        -> InputWaySelectorType4StreamingAlgorithm state
        -> MultiwayInputType4StreamingAlgorithm state
        -> [Private_Output3 state]
    g std_state selector inputss
        = case maybe_consume_input stateT selector inputss of
            Just (input, inputss') -> h std_state selector input inputss'
            Nothing -> []
    stateT = TheType :: forall. TheType state

    h :: forall. Standard state
        -> InputWaySelectorType4StreamingAlgorithm state
        -> InputType4StreamingAlgorithm state
        -> MultiwayInputType4StreamingAlgorithm state
        -> [Private_Output3 state]
    h std_state selector input inputss'
        = (Nothing, std_state', inputss') : f std_state' inputss'
        where
            nonstd_state' = update_after_consume (unStandard std_state) selector input
            std_state' = mkStandard nonstd_state'



