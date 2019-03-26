
-- how to reconstruct a given red-black-tree 
--    using only insert_at_leaf/remove_leaf_and_its_parent
--    begin from an empty tree (i.e. leaf_root)??

data RBT_PlainNodeT e = Leaf 
                              | Black e (RBT_PlainNodeT e) (RBT_PlainNodeT e)
                              | Red e (RBT_PlainNodeT e) (RBT_PlainNodeT e)

data RBT_IteratorT n = RBT_IteratorT {
        plain :: n,
        parent_info :: Maybe (Bool, (RBT_IteratorT n))
    }
    -- Bool :: is_self_a_right_child



-- n c e : node color entity
class RBT_Node n e | n -> e where
    unpack :: n -> Maybe (Bool, e, n, n) -- is_black::Bool
    
    BLACK = True
    RED = False
    is_leaf n = (unpack n) == Nothing
    LEAF = pack Nothing
    color n = case unpack n of
        Nothing -> BLACK
        Just (c, _, _, _) -> c

    is_black :: node -> Bool
    is_black n = color n
    is_red = not . is_black
    
    -- unsafe
    entity n = case unpack n of
        Just (_, e, _, _) -> e
        -- Nothing -> undefined
    left n = case unpack n of
        Just (_, _, left_child, _) -> left_child
    right n = case unpack n of
        Just (_, _, _, right_child) -> right_child







class RBT_Node n e => RBT_Node_Plain n e where
    pack :: Maybe (Bool, e, n, n) -> n
    

class RBT_Node n e => RBT_Node_Parented n e where
    mk_parented 




instance RBT_Node n e => RBT_Node (RBT_IteratorT n) e where
    LEFT = False
    RIGHT = True
    unpack it = case unpack $ plain it of
        Nothing -> Nothing
        Just (c, e, _, _) -> Just (c, e, left it, right it)
    pack Nothing -> 

instance RBT_Node_Plain (RBT_PlainNodeT e) e where
    unpack Leaf = Nothing
    unpack Black e left right = Just (BLACK, e, left, right)
    unpack Red e left right = Just (RED, e, left, right)
    pack Nothing = Leaf
    pack Just (b, e, left, right) = if b then Black e left right else Red e left right




-- leaf -> entity -> root
insert_at_leaf :: RBT_PlainNode n e => n -> e -> n



















