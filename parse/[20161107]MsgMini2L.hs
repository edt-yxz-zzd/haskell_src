{-# LANGUAGE FlexibleContexts
    , FlexibleInstances
    , TypeSynonymInstances
    , UndecidableInstances
    , OverlappingInstances
    , TypeFamilies
    , Rank2Types
    , ScopedTypeVariables
    , TypeOperators
    #-}


-- see "parser design.txt"
-- common mistake:
--      1) once word = r"\S+" => r"(?![MI])\S+" => not any keyword
--      2) many (try xxx) v.s. many xxx
--      3) try (string xxx) v.s string xxx
{-
*Main> P.parse (many $ char 'a' >> char 'a') "" "aaa"
Left (line 1, column 4):
unexpected end of input
expecting "a"

*Main> P.parse (many $ string "ab") "" "a"
Left (line 1, column 1):
unexpected end of input
expecting "ab"

*Main> P.parse (skipMany $ string "ab") "" "a"
Left (line 1, column 1):
unexpected end of input
expecting "ab"

-}

---- short hand
-- i - interface; 
-- im - importer; ex - exporter  -- Msg
-- ims - importers; exs - exporters;  -- [] or Set
-- stmt - statement
------- run script: runghc MsgMiniL.hs  Earley.MsgMini.txt


import NamedTuple
import SeedUtils_Parsec
import MonadLogger
import ToDoc
import Control.Monad
import Prelude hiding (log)
import Text.PrettyPrint hiding (char, space)
import Text.Parsec hiding (parse) -- , many, many1)
import Text.Parsec.String
import qualified Text.Parsec as P
import System.Environment
import qualified Data.Map as M -- (Map)
import qualified Data.Set as S -- (Set)
import qualified Data.List as L
import Data.Map (Map)
import Data.Set (Set)
import SeedUtils
    ( easy_productive_closure
    , group_pairs
    , group_pairs__set
    , merge_x_ys_pairs__set
    , swap_pair_in_ls
    )

main = do
    fnames <- getArgs
    show_files fnames

show_files = mapM_ show_file -- sequence_ . fmap show_file
show_file fname = do
    print fname
    r <- parse fname
    -- print r
    case r of 
        Left _ -> print r
        Right ls -> do
            print $ to_doc ls
            cr <- compile ls
            print $ to_doc cr
            let case_strs = compile_result2case_strs ls cr
            print $ to_doc case_strs

    -- show_files fs

data CaseStrs = CaseStrs{ to_iimport_sets :: String
                        , to_iexports :: String
                        , to_pushouts :: String
                        , to_outmsgs :: String
                        }
instance ToDoc CaseStrs where
    to_doc CaseStrs { to_iimport_sets = im_case_str
             , to_iexports = ex_case_str
             , to_pushouts = pout_case_str
             , to_outmsgs = out_case_str
             } =
        to_doc ("to_iimport_sets" -: text im_case_str //
                "to_iexports" -: text ex_case_str //
                "to_pushouts" -: text pout_case_str ///
                "to_outmsgs" -: text out_case_str
                )
compile_result2case_strs
    ::  ( MConstructor ~ msg, IConstructor ~ i
        , Set msg ~ msgs, Set i ~ iset)
        => MsgMiniL -> CompileResult i msg iset msgs -> CaseStrs
compile_result2case_strs m cr =
    -- methods of instance MsgLocalAction i msg (Obj i msg)
    CaseStrs { to_iimport_sets =
                "process1__to_iimport_sets msg = " ++ im_case_str
             , to_iexports =
                "process1__to_iexports msg = " ++ ex_case_str
             , to_pushouts =
                "process1__to_pushouts i = " ++ pout_case_str
             , to_outmsgs =
                "process2__to_outmsgs im i2ex =\n" ++ out_case_str
             } where
    ls = m
    mc_strs = unique_and_show_msgs $ collect_msgs__ls ls
    ic_strs = unique_and_show_interfaces $ collect_interfaces__ls ls
    --print . to_doc $ mc_strs
    --print . to_doc $ ic_strs
    mc2str = ctorstr2map mc_strs
    ic2str = ctorstr2map ic_strs
    icset2strset = S.map (ic2str M.!)
    mcset2strset = S.map (mc2str M.!)
    im2isets' = im2isets cr
    im2istrsets = M.map (S.map icset2strset) im2isets'
    imstr2istrsets = M.mapKeys (mc2str M.!) im2istrsets
    strls2str = (\x->"[" ++ x ++ "]") . L.intercalate ", "
    strset2str = ("S.fromList " ++) . strls2str . S.toList
    strsetset2str = (strls2str . fmap strset2str . S.toList)
    strset2str' = strls2str . S.toList
    strsetset2str' = strls2str . fmap strset2str' . S.toList
    imstr2isets_str = M.insert "_" "[]" $
        M.map strsetset2str' imstr2istrsets
    to_case_body_str str2str = M.foldrWithKey f "" str2str where
            f k s b = "    " ++ k ++ " -> " ++ s ++ "\n" ++ b
    im_case_body_str = to_case_body_str imstr2isets_str
    im_case_str = "return . fmap S.fromList $ case msg of\n"
                    ++ im_case_body_str
    --print . to_doc $ imstr2istrsets
    --print $ to_doc imstr2isets_str
    --putStrLn im_case_str

    ex2strls = M.map (S.toList . icset2strset) $ ex2iset cr
    ex2strls' = M.mapKeys (mc2str M.!) ex2strls
    ex2ls_str = M.insert "_" "[]" $ M.map strls2str ex2strls'
    ex_case_body_str = to_case_body_str ex2ls_str
    ex_case_str = "return $ case msg of\n"
                    ++ ex_case_body_str
    --print $ to_doc ex2ls_str
    --putStrLn ex_case_str

    i2outs = M.map (S.toList . mcset2strset) $ i2pushouts cr
    i2outs' = M.mapKeys (ic2str M.!) i2outs
    i2outs_str = M.insert "_" "[]" $ M.map strls2str i2outs'
    pout_case_body_str = to_case_body_str i2outs_str
    pout_case_str = "return $ case i of\n"
                        ++ pout_case_body_str
    --print $ to_doc i2outs_str
    --putStrLn pout_case_str

    im_i2ex2outs' = im_i2ex2outs cr
    im_iset2i2ex_ls' = im_iset2i2ex_ls cr
    mcls2str ls = strls2str $ fmap (mc2str M.!) ls
    icls2str ls = strls2str $ fmap (ic2str M.!) ls
    im_ils_exs2strpair im ils exs = (mcls2str exs, mcls2str outs) where
        outs = S.toList $ im_i2ex2outs' M.! (im, M.fromList $ zip ils exs)
    im_ils_exs2line im ils exs = a ++ " -> " ++ b ++ "\n" where
        (a, b) = im_ils_exs2strpair im ils exs
    get_values = fmap snd . M.toAscList
    im_ils2lins im ils =
        [ im_ils_exs2line im ils $ get_values i2ex
        | i2ex <- im_iset2i2ex_ls' M.! (im, S.fromList ils)]
    im_ils2case_lines im ils =
        (icls2str ils ++ " -> case exs of\n")
        : fmap ("  " ++) (im_ils2lins im ils)
    im2lines im = concat
        [im_ils2case_lines im $ S.toAscList iset
        | iset <- S.toList $ im2isets' M.! im]
    im2case_lines im = 
        ((mc2str M.! im) ++ " -> case keys of\n")
        : fmap ("  " ++) (im2lines im)
    out_case_body_lines = concat
        [im2case_lines im | im <- S.toList $ importers cr]
    out_case_lines =
        [ "let items = M.toAscList i2ex\n"
        , "    keys = fmap fst items\n"
        , "    exs = fmap snd items\n"
        , "in  return $ case im of\n"
        ] ++ fmap ("  " ++) out_case_body_lines
    out_case_str = concat out_case_lines
    --putStrLn out_case_str

-- parse :: String -> Main
parse = parseFromFile msg_mini_main




{-
 - use Python to comile and verify instead of Haskell
 -      not need - list comprehension
---------------- compile
-- c - compiled
-}



instance ToDoc TopStmt where
    to_doc (ImportView msg n stmts) =
        py_call2doc "ImportView" (A2 msg stmts)
    to_doc (InterfaceDecl i exs) =
        py_call2doc "InterfaceDecl" (A2 i exs)
    to_doc (PushFwd i outs) =
        py_call2doc "PushFwd" (A2 i outs)
instance ToDoc ImplementedByStmt where
    to_doc (ImplementedByStmt exs outs) = py_call2doc "ImplementedByStmt" (A2 exs outs)
instance ToDoc ImportStmt where
    to_doc (ImportStmt iset stmts) = py_call2doc "ImportStmt" (A2 iset stmts)

instance ToDoc Msg where
    to_doc (Msg c ls) = py_call2doc "Msg" $ A2 c ls
instance ToDoc Interface where
    to_doc (Interface c ls) = py_call2doc "Interface" $ A2 c ls




-------------------
--
--

data CompileResult i msg iset msgs =
    CompileResult   { srcs :: msgs
                    , pure_srcs :: msgs
                    , pure_outs :: msgs

                    , importers :: msgs
                    , exporters_in_use :: msgs
                    , unknown_msgs :: msgs
                    , almost_all_outs :: msgs
                    , all_msgs_in_use :: msgs
                    , all_msgs_in_use_size :: Int

                    , i2pushouts :: Map i msgs
                    , im2isets :: Map msg (Set iset)
                    , im_iset2i2ex_ls :: Map (msg, iset) [Map i msg]
                    , im_i2ex2outs :: Map (msg, Map i msg) msgs
                    , iset2importers :: Map iset msgs
                    , i2exporters :: Map i msgs
                    , ex2iset :: Map msg iset
                    , iset_in_use :: iset
                    , unused_iset :: iset
                    , decl_iset :: iset
                    , decl_iset_size :: Int
                    }

instance (ToDoc i, ToDoc msg, ToDoc iset, ToDoc msgs)
    => ToDoc (CompileResult i msg iset msgs) where
    to_doc CompileResult{ srcs = srcs
                        , pure_srcs = pure_srcs
                        , pure_outs = pure_outs
                        , importers = importers
                        , exporters_in_use = exporters_in_use
                        , unknown_msgs = unknown_msgs
                        , almost_all_outs = almost_all_outs
                        , all_msgs_in_use = all_msgs_in_use
                        , all_msgs_in_use_size = all_msgs_in_use_size

                        , i2pushouts = i2pushouts
                        , im2isets = im2isets
                        , im_iset2i2ex_ls = im_iset2i2ex_ls
                        , im_i2ex2outs = im_i2ex2outs
                        , iset2importers = iset2importers
                        , i2exporters = i2exporters
                        , ex2iset = ex2iset
                        , iset_in_use = iset_in_use
                        , unused_iset = unused_iset
                        , decl_iset = decl_iset
                        , decl_iset_size = decl_iset_size } =
            to_doc ("srcs" -: srcs //
                    "pure_srcs" -: pure_srcs //
                    "pure_outs" -: pure_outs //
                    "importers" -: importers //
                    "exporters_in_use" -: exporters_in_use //
                    "unknown_msgs" -: unknown_msgs //
                    "almost_all_outs" -: almost_all_outs //
                    "all_msgs_in_use" -: all_msgs_in_use //
                    "all_msgs_in_use_size" -: all_msgs_in_use_size //

                    "i2pushouts" -: i2pushouts //
                    "im2isets" -: im2isets //
                    "im_iset2i2ex_ls" -: im_iset2i2ex_ls //
                    "im_i2ex2outs" -: im_i2ex2outs //
                    "iset2importers" -: iset2importers //
                    "i2exporters" -: i2exporters //
                    "ex2iset" -: ex2iset //
                    "iset_in_use" -: iset_in_use //
                    "unused_iset" -: unused_iset //
                    "decl_iset" -: decl_iset ///
                    "decl_iset_size" -: decl_iset_size
                    )


compile :: ( Monad m, MConstructor ~ msg, IConstructor ~ i
           , Set msg ~ msgs, Set i ~ iset
           --, m ~ IO
           ) =>
            {-
    MsgMiniL -> m ( msgs :-: msgs :-: msgs
                :-: msgs :-: msgs :-: msgs :-: msgs :-: msgs :-: Int

                :-: Map i msgs
                :-: Map msg (Set iset)
                :-: Map (msg, iset) [Map i msg]
                :-: Map (msg, Map i msg) msgs
                :-: Map iset msgs :-: Map i msgs
                :-: Map msg iset
                :-: iset :-: iset :-: iset :-:: Int :$: String
                , CompileResult i msg iset msgs) -}
    MsgMiniL -> m (CompileResult i msg iset msgs)
compile m = do
    i2pushouts <- to_i2pushouts m
    im_i2ex2outs <- to_im_i2ex2outs m -- {(im, {i:ex}):{out}}
    let im_i2ex_ls = M.keys im_i2ex2outs
    let im_iset_i2ex_ls =   [ ((im, M.keysSet i2ex), i2ex)
                            | (im, i2ex) <- im_i2ex_ls]
    let im_iset2i2ex_ls = group_pairs im_iset_i2ex_ls
        -- {(im, {i}):[{i:ex}]}
    let im_iset2num_i2ex = M.map (toInteger . length) im_iset2i2ex_ls
    let im2isets = group_pairs__set $ M.keys im_iset2num_i2ex
    let rules = make_rules im_i2ex2outs i2pushouts im2isets
    let builtin_src = ">-"
    let builtin_srcs = S.singleton builtin_src
    let (fines, bads) = easy_productive_closure rules builtin_srcs
    unless (S.null bads) . fail $ "nonreachable msgs: " ++ show bads

    let all_msgs_in_use = fines
    i2exporters <- to_i2exporters m -- {i:{ex}}
    let ex2iset = group_pairs__set
                    [(ex, i) | (i, exs) <- M.toList i2exporters
                             , ex <- S.toList exs]
    -- some exporters in i2exporters may not in all_msgs_in_use
    let iset_in_use = S.fromList [i | (im, i2ex) <- M.keys im_i2ex2outs
                                    , i <- M.keys i2ex]
    let decl_iset = M.keysSet i2exporters
    let iset_without_decl = iset_in_use S.\\ decl_iset
    unless (S.null iset_without_decl) . fail $
        "interfaces without decl: " ++ show iset_without_decl
    -- decl_iset >= iset_in_use
    -- im_i2ex2outs, all_msgs_in_use
    -- i2exporters, ex2iset, iset_in_use, decl_iset

    -- let im_i2ex_ls = M.keys im_i2ex2outs
    let err_im_i2ex_ls = filter (\(im, i2ex) ->
            not $ M.isSubmapOfBy S.member i2ex i2exporters) im_i2ex_ls
    unless (null err_im_i2ex_ls) . fail $
        "import view :: exporter of interface without decl: " ++
        show err_im_i2ex_ls
    -- decl_i2ex >= i2ex_in_use

    -- let im_iset_i2ex_ls =   [ ((im, M.keysSet i2ex), i2ex)
    --                         | (im, i2ex) <- im_i2ex_ls]
    -- let im_iset2i2ex_ls = group_pairs im_iset_i2ex_ls
        -- {(im, {i}):[{i:ex}]}
    -- let im_iset2num_i2ex = M.map (toInteger . length) im_iset2i2ex_ls
    let i2num_ex = M.map (toInteger . S.size) i2exporters
    let err_im_iset2n = M.filterWithKey f im_iset2num_i2ex where
        f (im, iset) num_i2ex = calc_num_i2ex i2num_ex iset /= num_i2ex
    unless (M.null err_im_iset2n) . fail $
        "import view :: interfaces not come with all exporters: " ++
        show err_im_iset2n
    -- i in use ==>> decl_i2ex == i2ex_in_use
    -- there are some decl interfaces not in use
    --      and some of their exporters not in use
    --          (i.e. not in all_msgs_in_use)
    --



    let unused_iset = decl_iset S.\\ iset_in_use
    let all_msgs = S.unions $ all_msgs_in_use : M.elems i2exporters
    let unknown_msgs = all_msgs S.\\ all_msgs_in_use

    -- let im2isets = group_pairs__set $ M.keys im_iset2num_i2ex
    let iset2importers = group_pairs__set . swap_pair_in_ls $
                        M.keys im_iset2num_i2ex
    let builtin_srcsdecl_key = (builtin_src, M.empty)
    let srcs = M.findWithDefault S.empty builtin_srcsdecl_key im_i2ex2outs
    let iset2exporters_ = M.fromList
            [ (iset, S.unions . M.elems $
                        set_intersect_map iset i2exporters)
            | iset <- M.keys iset2importers]
    let err_im_iset_ls = [ (im, iset)
            | (im, isets) <- M.toList im2isets, iset <- S.toList isets
            , S.member im $ iset2exporters_ M.! iset]
    unless (null err_im_iset_ls) . fail $
        "importer should not be exporter of interface it imported" ++
        "(i.e. ERROR:importer -< interface <- importer): " ++
        show err_im_iset_ls
    -- unused_iset, unknown_msgs, im2isets, iset2importers, srcs
    -- im_iset2i2ex_ls



    -- pure_outs = almost_all_outs - importers - exporters_in_use
    --          may include srcs
    -- pure_srcs = all_msgs_in_use - almost_all_outs - ">-"
    -- see decl_iset: let all_interfaces = M.keysSet i2exporters
    let importers = merge_values iset2importers
    let decl_exporters = merge_values i2exporters -- include unused
    -- let exporters_in_use = ERROR: all_exporters S.\\ unused_msgs
    let exporters_in_use = merge_values $
            M.filterWithKey (\i exs -> i `S.member` iset_in_use) i2exporters
    let all_pushouts = merge_values i2pushouts
        add_all_pushouts msgs = msgs `S.union` all_pushouts
    let all_outs_ = add_all_pushouts $ merge_values im_i2ex2outs
            -- include srcs
    let all_outs_exclude_builtin_src = -- may include srcs
            add_all_pushouts . merge_values $
                M.delete builtin_srcsdecl_key im_i2ex2outs
    let almost_all_outs = all_outs_exclude_builtin_src
    let pure_outs = almost_all_outs S.\\ importers S.\\ exporters_in_use
    let pure_srcs = S.delete builtin_src $
            all_msgs_in_use S.\\ almost_all_outs
    unless (pure_srcs `S.isSubsetOf` srcs) . fail $
        "logic error: not pure_srcs <= srcs :" ++
        show pure_srcs ++ "   " ++ show srcs
    -- pure_srcs <= srcs
    -- importers, decl_exporters, exporters_in_use
    --      almost_all_outs, pure_outs, pure_srcs



    -- im_i2ex2outs, all_msgs_in_use
    -- i2exporters, ex2iset, iset_in_use, decl_iset
    -- unused_iset, unknown_msgs, im2isets, iset2importers, srcs
    -- im_iset2i2ex_ls
    -- importers, exporters_in_use
    --      almost_all_outs, pure_outs, pure_srcs
    let cr = CompileResult  { srcs = srcs
                        , pure_srcs = pure_srcs
                        , pure_outs = pure_outs
                        , importers = importers
                        , exporters_in_use = exporters_in_use
                        , unknown_msgs = unknown_msgs
                        , almost_all_outs = almost_all_outs
                        , all_msgs_in_use = all_msgs_in_use
                        , all_msgs_in_use_size = S.size all_msgs_in_use

                        , i2pushouts = i2pushouts
                        , im2isets = im2isets
                        , im_iset2i2ex_ls = im_iset2i2ex_ls
                        , im_i2ex2outs = im_i2ex2outs
                        , iset2importers = iset2importers
                        , i2exporters = i2exporters
                        , ex2iset = ex2iset
                        , iset_in_use = iset_in_use
                        , unused_iset = unused_iset
                        , decl_iset = decl_iset
                        , decl_iset_size = S.size decl_iset }


    return cr -- (result3, cr)
  where
    make_rules im_i2ex2outs i2pushouts im2isets = group_pairs $
        [ (out, im : M.elems i2ex)
        | ((im, i2ex), outs) <- M.toList im_i2ex2outs
        , out <- S.toList outs] ++
        [ (out, [im]) | (im, pushouts) <- M.toList im2pushouts
        , out <- pushouts] where
        im2iset_ = M.map (S.unions . S.toList) im2isets
        iset2pushouts iset =
            merge_values $ iset `set_intersect_map` i2pushouts
        im2pushouts = M.map (S.toList . iset2pushouts) im2iset_
    merge_values :: Ord a => Map k (Set a) -> Set a
    merge_values = S.unions . M.elems
    calc_num_i2ex :: Ord i => Map i Integer -> Set i -> Integer
    calc_num_i2ex i2num_ex iset = foldr (*) 1 $
        fmap (\i -> toInteger $ M.findWithDefault 0 i i2num_ex) $
        S.toList iset
    set_intersect_map :: Ord k => Set k -> Map k a -> Map k a
    set_intersect_map set map = M.fromList ls where
        subset = M.keysSet map `S.intersection` set
        ls = [(k, map M.! k) | k <- S.toList subset]


to_i2pushouts :: Monad m =>
    MsgMiniL -> m (Map IConstructor (Set MConstructor))
to_i2pushouts m = return $ merge_x_ys_pairs__set
    [(get_interface_ctor i, get_msg_ctors outs) | PushFwd i outs <- m]
to_im_i2ex2outs :: Monad m =>
    MsgMiniL -> m (Map  (MConstructor, Map IConstructor MConstructor)
                        (Set MConstructor))
to_im_i2ex2outs m = r where
    -- @return outs may be empty
    -- no multi-interfaces
    im_views = [(im, stmts) | ImportView im n stmts <- m]
    qfs_ = flatten_import_view im_views
    fs_ = [ (   ( get_msg_ctor im
                , get_interface_ctors i_ls
                , get_msg_ctors exs
                )
            , get_msg_ctors outs)
          | (im, i_ls, exs, outs) <- qfs_]
    err1 = filter (\((im, i_ls, exs), _)-> length i_ls /= length exs) fs_
    stmt1 = unless (null err1) $
        fail $ "logic error: len(i_ls) != len(exs) ==>> parser error"

    -- ss_ :: [(im, i2ex), outs]
    ss_ =   [ ((im, M.fromList $ zip i_ls exs), outs)
            | ((im, i_ls, exs), outs) <- fs_]
    err2 =  [ (im, i_ls, exs, outs)
            | (((im, i_ls, exs), outs), ((im_, i2ex), outs_))
              <- zip fs_ ss_
            , length i_ls /= M.size i2ex]
    stmt2 = unless (null err2) $
        fail $ "duplicate interfaces: " ++ show err2
    im_i2ex2outs = merge_x_ys_pairs__set ss_

    r = do
        stmt1
        stmt2
        return im_i2ex2outs


--[im, [i, [ex, [out]]]]
flatten_import_view :: [(ImporterEx, [ImportStmt])]
                    -> [(ImporterEx, [Interface], [Exporter], [OutMsg])]
flatten_import_view = concat . fmap f where
    f (im, stmts) = [(im, i_ls, exs, outs) |
                        ImportStmt i_ls exstmts <- stmts,
                        ImplementedByStmt exs outs <- exstmts]





to_i2exporters :: Monad m =>
    MsgMiniL -> m (Map IConstructor (Set MConstructor))
to_i2exporters m = r where
    -- get i_decl.exs
    -- merge i->exs
    -- get im_view
    -- flatten
    -- merge (im, i)->exs
    -- ==>> i->(im,exs)
    -- i->compare exs
    i_exs_ls = [(get_interface_ctor i, get_msg_ctors exs) |
                InterfaceDecl i exs <- m]
    decl_i2exs = merge_x_ys_pairs__set i_exs_ls
    r = return decl_i2exs
{-
    im_views = [(im, stmts) | ImportView im stmts <- m]
    qfs_ = flatten_import_view im_views
    fs_ = [((get_msg_ctor im, get_interface_ctor i), get_msg_ctor ex) |
           (im, i, ex, _) <- qfs_]
    fs2_ = M.toList $ group_pairs fs_ -- :: [((im, i), [ex])]
    fs = [(i, (im, S.fromList exs)) | ((im, i), exs) <- fs2_]
    i2im_exs_ls = group_pairs fs -- {i:[(im, exs)]}
    default_i2exs = M.map (snd . head) i2im_exs_ls
    i2exs = M.union decl_i2exs default_i2exs -- set default if no decl
    i2err_im_exs_ls = M.intersectionWith
            (\ls exs -> [(im, exs') | (im, exs') <- ls, exs' /= exs])
            i2im_exs_ls i2exs
    i2errs = M.filter (not . null) i2err_im_exs_ls
    i2i_exs_errs = M.intersectionWithKey (,,) i2exs i2errs
    i_exs_errs_ls = M.elems i2i_exs_errs -- [(i, {ex}, [(im, {ex})])]
    err_str = "mismatch: interface -> exporters\n" ++ show i_exs_errs_ls
    r = if null i_exs_errs_ls
        then return i2exs
        else fail err_str
-}

get_msg_ctors :: Functor f => f Msg -> f MConstructor
get_interface_ctors :: Functor f => f Interface -> f IConstructor
get_msg_ctors = fmap get_msg_ctor
get_interface_ctors = fmap get_interface_ctor
get_msg_ctor (Msg t _) = t
get_interface_ctor (Interface t _) = t
{-
msg2ctor :: Msg -> MConstructor
msg2ctor (Msg ctor _) = ctor
interface2ctor :: Interface -> IConstructor
interface2ctor (Interface ctor _) = ctor
-}


collect_import_stmts :: MsgMiniL -> [ImportStmt]
collect_import_stmts m =
    concat [stmts | ImportView _ _ stmts <- m]
collect_importedby_stmts :: MsgMiniL -> [ImplementedByStmt]
collect_importedby_stmts m =
    concat [stmts | ImportStmt _ stmts <- collect_import_stmts m]
collect_msgs__ls :: MsgMiniL -> [Msg]
collect_msgs__ls m = 
    concat $ [im | ImportView im _ _ <- m] :
    [exs ++ outs | ImplementedByStmt exs outs <- collect_importedby_stmts m] ++
    [exs | InterfaceDecl _ exs <- m] ++
    [outs | PushFwd _ outs <- m]
collect_interfaces__ls :: MsgMiniL -> [Interface]
collect_interfaces__ls m =
    concat $ [i | InterfaceDecl i _ <- m] : [i | PushFwd i _ <- m] :
             [ls | ImportStmt ls _ <- collect_import_stmts m]

unique_by_set :: Ord a => [a] -> [a]
unique_by_set = S.toList . S.fromList
str_msg :: Msg -> String
str_msg (Msg ctor args) = unwords $ ctor : args
str_interface :: Interface -> String
str_interface (Interface ctor args) = unwords $ ctor : args

unique_and_show_msgs :: [Msg] -> [String]
unique_and_show_msgs = fmap str_msg . unique_by_set
unique_and_show_interfaces :: [Interface] -> [String]
unique_and_show_interfaces = fmap str_interface . unique_by_set

ctorstr2map :: [String] -> Map String String
ctorstr2map strs = M.fromList [(head $ words str, str) | str <- strs]
---------------- parse
type MsgMiniL = [TopStmt]
data TopStmt    = ImportView ImporterEx Int [ImportStmt]
                | InterfaceDecl Interface [Exporter]
                | PushFwd Interface [OutMsg]
    deriving(Show, Read, Eq, Ord)

type SrcMsg = Msg
type OutMsg = Msg
type Exporter = Msg
type ImporterEx = Msg -- Either String Importer -- left ">-"
-- type Importer = Msg
data ImportStmt = ImportStmt [Interface] [ImplementedByStmt]
    deriving(Show, Read, Eq, Ord)
data ImplementedByStmt = ImplementedByStmt [Exporter] [OutMsg]
    deriving(Show, Read, Eq, Ord)

data Msg = Msg MConstructor [Word]
    deriving(Show, Read, Eq, Ord)
data Interface = Interface IConstructor [Word]
    deriving(Show, Read, Eq, Ord)

type MConstructor = String
type IConstructor = String
type Word = String





---------------------------------------
any_keyword :: Stream s m Char => ParsecT s u m String
line_comment, mlines_comment, ignore_, ignores
    :: Stream s m Char => ParsecT s u m ()
wrap :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a

word, word_ :: Stream s m Char => ParsecT s u m String
word_prefix
    :: Stream s m Char => ParsecT s u m a -> ParsecT s u m String
word_notprefix
    :: (Stream s m Char, Show a) => ParsecT s u m a -> ParsecT s u m String



line_comment = skip $ keyword "--" >> skip_line
mlines_comment = skip $ keyword "{-" >>
    manyTill anyChar (try $ space >> keyword "-}")
ignore_ = skipMany1 space <|> line_comment <|> mlines_comment
    -- no <?> "ignore"
ignores = skipMany $ ignore_ -- without "try"! try ignore_



wrap a = discard_right a ignores


any_keyword = choice $ fmap keyword $ words "; -< <- --> >-"
word_ = wrap $ notwell_end >> notFollowedBy any_keyword >>
        -- not_ahead well_end >>
        -- notFollowedBy well_end >>
    manyTill anyChar (try well_end)
word_prefix prefix = lookAhead prefix >> word_
word_notprefix notprefix = notFollowedBy notprefix >> word_
---------------------------------------


-- -< <- --> ; >- -- {- -}
-- NOTE: "-<" and "-->" overlap

msg_mini_main :: Stream s m Char => ParsecT s u m MsgMiniL
top_stmt, import_view, interface_decl, push_fwd
    :: Stream s m Char => ParsecT s u m TopStmt
import_stmt
    :: Stream s m Char => Int -> ParsecT s u m ImportStmt
implementedby_stmt
    :: Stream s m Char => Int -> ParsecT s u m ImplementedByStmt
op_outmsg, op_exporter, msg, importer_ex --, op_importer
    :: Stream s m Char => ParsecT s u m Msg
op_interface, interface
    :: Stream s m Char => ParsecT s u m Interface
m_constructor, i_constructor
    :: Stream s m Char => ParsecT s u m String


msg_mini_main = between ignores eof . many $
                    wrap (keyword ";") >> top_stmt
top_stmt    =   import_view
            <|> interface_decl
            <|> push_fwd
            <?> "top_stmt"

push_fwd = do
    i <- op_interface
    outs <- many1 op_outmsg
    return $ PushFwd i outs
interface_decl = do
    i <- interface
    exs <- many1 op_exporter
    return $ InterfaceDecl i exs
import_view = do
    -- im <- msg >>= Right <|> wrap (keyword ">-") >>= Left
    im <- importer_ex
    ls <- lookAhead $ many0 op_interface
    let n = length ls
    stmts <- many1 $ import_stmt n
    return $ ImportView im n stmts

importer_ex =   msg
            <|> (wrap (keyword ">-") >>= return . flip Msg [])
            <?> "msg or \">-\""

import_stmt n = do
    ls <- count n op_interface
    stmts <- many1 $ implementedby_stmt n
    return $ ImportStmt ls stmts

implementedby_stmt n = do
    exs <- count n op_exporter
    outs <- (if n > 0 then many0 else many1) op_outmsg
    return $ ImplementedByStmt exs outs





op_outmsg = wrap (keyword "-->") >> msg
op_exporter = wrap (keyword "<-") >> msg
-- op_importer = wrap (keyword ">-") >> msg
op_interface = wrap (keyword "-<") >> interface
msg = do
    mc <- m_constructor
    ws <- many0 word
    return $ Msg mc ws
interface = do
    ic <- i_constructor
    ws <- many0 word
    return $ Interface ic ws

m_constructor = word_prefix $ char 'M'
i_constructor = word_prefix $ char 'I'
word = word_notprefix $ oneOf "MI"




-- -}
-- -}
-- -}
-- -}

