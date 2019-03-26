
module ADT.IException
where

class IUnexpectedException e where
    -- a "not" operation
    -- used in place like Parsec::unexpected
    to_unexpected :: e -> e


