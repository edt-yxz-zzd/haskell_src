{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}




module Graph
    ( module Graph
    , module Graph_base
    )
where
import Graph_base
import Container_base_static
import ExplainEx
import Prelude hiding (lookup)
import SeedUtils (unjust)


{-

planar - ordered set
half edge
nonnull graph


-}




-- type Pair a = (a, a)
data EndsViewEx pair set2 vtx = DEnds (pair vtx vtx) | UEnds (set2 vtx)
type EndsView = EndsViewEx (,) MultiSet_Len2
type EndsViewNoDuplicates = EndsViewEx (,) Set_Len2
type XEdge g = Either (DEdge g) (UEdge g)
class   --( Explain (Either (DEdge g) (UEdge g)) (Edge g)
        ( Eq (Vtx g), Eq (Edge g)
        , OpMember (Vtx g) (AllVtcs g)
        , OpMember (Vtx g) (Vtcs g)
        , OpMember (Edge g) (AllEdges g)
        , OpMember (Edge g) (Edges g)
        , OpMember (Edge g) (MultiEdges g)
        , Explain (EndsView (Vtx g)) (Ends g)
        )
    => Graph g where
    type Vtx g :: *
    type Edge g :: *
    type DEdge g :: *
    type UEdge g :: *
    type Vtcs g :: *
    type Edges g :: *
    type MultiEdges g :: *
    type AllVtcs g :: *
    type AllEdges g :: *
    type Ends g :: *
    vertices :: g -> AllVtcs g
    edges :: g -> AllEdges g
    neighbors :: g -> Vtx g -> Vtcs g
    incident_edges :: g -> Vtx g -> Edges g
    edge2ends :: g -> Edge g -> EndsView (Vtx g)
    edge2ends g = explain . edge2ends_ex g
    edge2ends_ex :: g -> Edge g -> Ends g
    edge2xedge :: g -> Edge g -> XEdge g
    xedge2edge :: g -> XEdge g
    ends2edges :: g -> EndsView (Vtx g) -> MultiEdges g

-- class (Graph g, Vtx g~Edge g) => XXX g




class (Graph g, NonNull (Vtx g) (AllVtcs g)) => NonNullGraph g
class (Graph g, Explain (DEdge g) (Edge g)) => DirectedGraph g where
class (Graph g, Explain (UEdge g) (Edge g)) => UndirectedGraph g where
class (Graph g, Optional (Edge g) (MultiEdges g))
    => WithoutMultiEdges g where
class (Graph g, Explain (EndsViewNoDuplicates (Vtx g)) (Ends g))
    => WithoutSelfLoop g
class (Graph g, Finite (Vtx g) (AllVtcs g), Finite (Edge g) (AllEdges g))
    => FiniteGraph g
class (Graph g, Natural (NumAllVtcs g), Finite (Vtx g) (AllVtcs g))
    => OpNumVertices g where
    type NumAllVtcs g :: *
    num_vertices :: g -> NumAllVtcs g
class (Graph g, Natural (NumAllEdges g), Finite (Edge g) (AllEdges g))
    => OpNumEdges g where
    type NumAllEdges g :: *
    num_edges :: g -> NumAllEdges g





class   ( Graph g
        , OpMember (Vtx g) (CC_Vtcs g)
        , NonNull (Vtx g) (CC_Vtcs g)
        , Container (CC_Vtcs g) (CCs g)
        , OptionalMapView (Vtx2CC g)
        , Vtx g ~ KeyType (Vtx2CC g)
        , CC_Vtcs g ~ ValueType (Vtx2CC g)
        )
    => OpConnectedComponents g where -- OpCCs
    -- connected components
    type CCs g :: *
    type CC_Vtcs g :: *
    type Vtx2CC g :: *
    connected_components_ex :: g -> (CCs g, Vtx2CC g)
    connected_components :: g -> CCs g
    connected_components = fst . connected_components_ex
class (OpConnectedComponents g, Optional (CC_Vtcs g) (CCs g))
    => Connected g
class   ( FiniteGraph g, WithoutSelfLoop g, WithoutMultiEdges g
        , Connected g) -- NonNullGraph??
    => SimpleGraph g
class (SimpleGraph g, DirectedGraph g) => SD_Graph g
class (SimpleGraph g, UndirectedGraph g) => SU_Graph g
class   ( DirectedGraph g, WithoutSelfLoop g
        , LinearElementsSet (Vtx g) (OrderedVtcs g)
        )
    => DAG g where
    -- directed acyclic graph
    type OrderedVtcs g :: *
    reversed_topological_ordering :: g -> OrderedVtcs g
        -- {a<-b, a<-c} |- [a, b, c] or [a, c, b]

class   ( DirectedGraph g
        , OpMember (Vtx g) (SCC_Vtcs g)
        , NonNull (Vtx g) (SCC_Vtcs g)
        , LinearElementsSet (SCC_Vtcs g) (OrderedSCCs g)
        , OptionalMapView (Vtx2SCC g)
        , Vtx g ~ KeyType (Vtx2SCC g)
        , SCC_Vtcs g ~ ValueType (Vtx2SCC g)
        )
    => OpSCCs g where
    -- strong connected components
    type OrderedSCCs g :: *
    type SCC_Vtcs g :: *
    type Vtx2SCC g :: *
    reversed_topological_ordered_sccs_ex :: g -> (OrderedSCCs g, Vtx2SCC g)
    reversed_topological_ordered_sccs :: g -> OrderedSCCs g
    reversed_topological_ordered_sccs =
        fst . reversed_topological_ordered_sccs_ex





class   ( Graph g
        , NonNull (Vtx g) (Path_Vtcs g)
        , Container (Path_Vtcs g) (PathsFromTo g)
        )
    => OpPathsFromTo g where
    -- path not null: [v] if u==v
    -- [v, v] if self-loop
    type PathsFromTo g :: *
    type Path_Vtcs g :: *
    paths_from_to :: Vtx g -> Vtx g -> PathsFromTo g
        -- [u ... v]
class   ( OpPathsFromTo g
        , Container (Path_Vtcs g) (PathsBetween g)
        )
    => OpPathsBetween g where
    type PathsBetween g :: *
    paths_between :: MultiSet_Len2 (Vtx g) -> PathsBetween g
        -- [u ... v] and [v ... u]
        -- [v, u] /= [u, v]
class   ( DAG g, OpMember (Vtx g) (Roots g), WithoutMultiEdges g
        , OpPathsBetween g
        , Optional (Path_Vtcs g) (PathsBetween g))
    => DiForest g where
    type Roots g :: *
    roots :: g -> Roots g
    cc_root :: g -> CC_Vtcs g -> Vtx g
class   ( DiForest g, Connected g
        , NonNullGraph g
        , Singleton (CC_Vtcs g) (CCs g)
        , Singleton (Vtx g) (Roots g)
        )
    => RootedTree g where
    root :: g -> Vtx g
class   ( UndirectedGraph g, WithoutSelfLoop g, WithoutMultiEdges g
        , Connected g, NonNullGraph g
        , OpPathsFromTo g, Singleton (Path_Vtcs g) (PathsFromTo g)
        )
    => UTree g where





class   ( Graph g, Eq (HEdge g), OpMember (HEdge g) (HEdges g)
        )
    => HEdged g where
    type HEdge g :: * -- half edge
    type HEdges g :: *
    edge2hedges :: g -> Edge g -> EndsViewNoDuplicates (HEdge g)
    hedge2edge :: g -> HEdge g -> Edge g
    hedge2vtx :: g -> HEdge g -> Vtx g
    incident_hedges :: g -> Vtx g -> HEdges g
    hedge2another :: g -> HEdge g -> HEdge g

class   ( HEdged g
        , OptionalMapView (PlanarEmbedding g)
        , KeyType (PlanarEmbedding g)~(Vtx g)
        , ValueType (PlanarEmbedding g)~(ClockwiseHEdges g)
        , ClockwiseElementsSet (HEdge g) (ClockwiseHEdges g)
        --, LinearElementsSet (HEdge g) (ClockwiseHEdges g)
        )
    => PlanarGraph g where
    type PlanarEmbedding g :: *
    type ClockwiseHEdges g :: *
    planar_embedding :: g -> PlanarEmbedding g
    vtx2clockwise_hedges
        :: g -> PlanarEmbedding g -> Vtx g -> ClockwiseHEdges g
    vtx2clockwise_hedges g em v =
        if not . member v $ vertices g then error "not vtx"
        else case unoptional $ lookup v em of
        Nothing -> error "not planar embedding"
        Just hes -> hes





class Graph g => Colored g where
    type Color g :: *
    get_color :: g -> Vtx g -> Color g
class Graph g => Weighted g where
    type Weight g :: *
    get_weight :: g -> Edge g -> Weight g




--
class Graph g => OpDynRemoveEdge g where
    remove_edge :: Edge g -> g -> Maybe g
    unsafe_remove_edge :: Edge g -> g -> g
    unsafe_remove_edge e = unjust . remove_edge e
    discard_edge :: Edge g -> g -> g
    discard_edge e g = maybe g id $ remove_edge e g
class Graph g => OpDynRemoveVtx g where
    remove_vtx :: Vtx g -> g -> Maybe g
    unsafe_remove_vtx :: Vtx g -> g -> g
    unsafe_remove_vtx v = unjust . remove_vtx v
    discard_vtx :: Vtx g -> g -> g
    discard_vtx v g = maybe g id $ remove_vtx v g


