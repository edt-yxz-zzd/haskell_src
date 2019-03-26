"clean_grammar.MsgMini2.txt"
[ImportView(Msg(">-", []),
            [ImportStmt([],
                        [ImplementedByStmt([],
                                           [Msg("MStart", ["n"]),
                                            Msg("MToken", ["t"]),
                                            Msg("MRule", ["n", "name", "[sym]"])])])]),
 ImportView(Msg("MToken", ["t"]),
            [ImportStmt([],
                        [ImplementedByStmt([], [Msg("MProductive", ["(Right", "t)"])])])]),
 InterfaceDecl(Interface("IProductive_Sym", ["sym"]),
               [Msg("MProductive", ["sym"])]),
 ImportView(Msg("MRule", ["n", "name", "[sym]"]),
            [ImportStmt([],
                        [ImplementedByStmt([],
                                           [Msg("MRuleTail", ["n", "name", "[sym]", "[sym]"])])])]),
 ImportView(Msg("MRuleTail", ["n", "name", "syms", "sym:[sym1]"]),
            [ImportStmt([Interface("IProductive_Sym", ["sym"])],
                        [ImplementedByStmt([Msg("MProductive", ["sym"])],
                                           [Msg("MRuleTail", ["n", "name", "syms", "[sym1]"])])])]),
 ImportView(Msg("MRuleTail", ["n", "name", "syms", "[]"]),
            [ImportStmt([],
                        [ImplementedByStmt([],
                                           [Msg("MProductiveRule", ["n", "name", "syms"])])])]),
 ImportView(Msg("MProductiveRule", ["n", "name", "syms"]),
            [ImportStmt([],
                        [ImplementedByStmt([], [Msg("MProductive", ["(Left", "n)"])])])]),
 ImportView(Msg("MStart", ["n"]),
            [ImportStmt([Interface("IProductive_Sym", ["sym"])],
                        [ImplementedByStmt([Msg("MProductive", ["sym"])],
                                           [Msg("MOutput_Start", ["n"]),
                                            Msg("MReachable", ["(Left", "n)"])])])]),
 ImportView(Msg("MReachable", ["sym"]),
            [ImportStmt([],
                        [ImplementedByStmt([],
                                           [Msg("MOutput_Sym", ["sym"]),
                                            Msg("MReachableLeft", ["n"])])])]),
 ImportView(Msg("MReachableLeft", ["n"]),
            [ImportStmt([Interface("IProductiveLeftXNameRights", ["n"])],
                        [ImplementedByStmt([Msg("MProductiveRule", ["n", "name", "syms"])],
                                           [Msg("MOutput_Rule", ["n", "name", "syms"]),
                                            Msg("MReachable", ["sym"])])])]),
 InterfaceDecl(Interface("IOutput_XRule", []),
               [Msg("MOutput_Rule", ["n", "name", "syms"])]),
 InterfaceDecl(Interface("IOutput_XSym", []),
               [Msg("MOutput_Sym", ["sym"])]),
 InterfaceDecl(Interface("IOutput_XStart", []),
               [Msg("MOutput_Start", ["n"])]),
 InterfaceDecl(Interface("IXStart", []), [Msg("MStart", ["n"])]),
 InterfaceDecl(Interface("IXToken", []), [Msg("MToken", ["t"])]),
 InterfaceDecl(Interface("IXRule", []),
               [Msg("MRule", ["n", "name", "syms"])]),
 InterfaceDecl(Interface("IProductiveLeftXNameRights", ["n"]),
               [Msg("MProductiveRule", ["n", "name", "syms"])])]
("srcs" -: {"MRule", "MStart", "MToken"},
 "pure_srcs" -: {"MRule", "MStart", "MToken"},
 "pure_outs" -: {"MOutput_Rule", "MOutput_Start", "MOutput_Sym"},
 "importers" -: {">-",
                 "MProductiveRule",
                 "MReachable",
                 "MReachableLeft",
                 "MRule",
                 "MRuleTail",
                 "MStart",
                 "MToken"},
 "exporters_in_use" -: {"MProductive", "MProductiveRule"},
 "unknown_msgs" -: {},
 "almost_all_outs" -: {"MOutput_Rule",
                       "MOutput_Start",
                       "MOutput_Sym",
                       "MProductive",
                       "MProductiveRule",
                       "MReachable",
                       "MReachableLeft",
                       "MRuleTail"},
 "all_msgs_in_use" -: {">-",
                       "MOutput_Rule",
                       "MOutput_Start",
                       "MOutput_Sym",
                       "MProductive",
                       "MProductiveRule",
                       "MReachable",
                       "MReachableLeft",
                       "MRule",
                       "MRuleTail",
                       "MStart",
                       "MToken"},
 "all_msgs_in_use_size" -: 12,
 "i2pushouts" -: {},
 "im2isets" -: {">-" : {{}},
                "MProductiveRule" : {{}},
                "MReachable" : {{}},
                "MReachableLeft" : {{"IProductiveLeftXNameRights"}},
                "MRule" : {{}},
                "MRuleTail" : {{}, {"IProductive_Sym"}},
                "MStart" : {{"IProductive_Sym"}},
                "MToken" : {{}}},
 "im_iset2i2ex_ls" -: {(">-", {}) : [{}],
                       ("MProductiveRule", {}) : [{}],
                       ("MReachable", {}) : [{}],
                       ("MReachableLeft",
                        {"IProductiveLeftXNameRights"}) : [{"IProductiveLeftXNameRights" : "MProductiveRule"}],
                       ("MRule", {}) : [{}],
                       ("MRuleTail", {}) : [{}],
                       ("MRuleTail",
                        {"IProductive_Sym"}) : [{"IProductive_Sym" : "MProductive"}],
                       ("MStart",
                        {"IProductive_Sym"}) : [{"IProductive_Sym" : "MProductive"}],
                       ("MToken", {}) : [{}]},
 "im_i2ex2outs" -: {(">-", {}) : {"MRule", "MStart", "MToken"},
                    ("MProductiveRule", {}) : {"MProductive"},
                    ("MReachable", {}) : {"MOutput_Sym", "MReachableLeft"},
                    ("MReachableLeft",
                     {"IProductiveLeftXNameRights" : "MProductiveRule"}) : {"MOutput_Rule",
                                                                            "MReachable"},
                    ("MRule", {}) : {"MRuleTail"},
                    ("MRuleTail", {}) : {"MProductiveRule"},
                    ("MRuleTail", {"IProductive_Sym" : "MProductive"}) : {"MRuleTail"},
                    ("MStart", {"IProductive_Sym" : "MProductive"}) : {"MOutput_Start",
                                                                       "MReachable"},
                    ("MToken", {}) : {"MProductive"}},
 "iset2importers" -: {{} : {">-",
                            "MProductiveRule",
                            "MReachable",
                            "MRule",
                            "MRuleTail",
                            "MToken"},
                      {"IProductiveLeftXNameRights"} : {"MReachableLeft"},
                      {"IProductive_Sym"} : {"MRuleTail", "MStart"}},
 "i2exporters" -: {"IOutput_XRule" : {"MOutput_Rule"},
                   "IOutput_XStart" : {"MOutput_Start"},
                   "IOutput_XSym" : {"MOutput_Sym"},
                   "IProductiveLeftXNameRights" : {"MProductiveRule"},
                   "IProductive_Sym" : {"MProductive"},
                   "IXRule" : {"MRule"},
                   "IXStart" : {"MStart"},
                   "IXToken" : {"MToken"}},
 "ex2iset" -: {"MOutput_Rule" : {"IOutput_XRule"},
               "MOutput_Start" : {"IOutput_XStart"},
               "MOutput_Sym" : {"IOutput_XSym"},
               "MProductive" : {"IProductive_Sym"},
               "MProductiveRule" : {"IProductiveLeftXNameRights"},
               "MRule" : {"IXRule"},
               "MStart" : {"IXStart"},
               "MToken" : {"IXToken"}},
 "iset_in_use" -: {"IProductiveLeftXNameRights", "IProductive_Sym"},
 "unused_iset" -: {"IOutput_XRule",
                   "IOutput_XStart",
                   "IOutput_XSym",
                   "IXRule",
                   "IXStart",
                   "IXToken"},
 "decl_iset" -: {"IOutput_XRule",
                 "IOutput_XStart",
                 "IOutput_XSym",
                 "IProductiveLeftXNameRights",
                 "IProductive_Sym",
                 "IXRule",
                 "IXStart",
                 "IXToken"},
 "decl_iset_size" -: 8)
("to_iimport_sets" -: process1__to_iimport_sets msg = return . fmap S.fromList $ case msg of
    >- -> [[]]
    MProductiveRule n name syms -> [[]]
    MReachable sym -> [[]]
    MReachableLeft n -> [[IProductiveLeftXNameRights n]]
    MRule n name syms -> [[]]
    MRuleTail n name syms sym:[sym1] -> [[], [IProductive_Sym sym]]
    MStart n -> [[IProductive_Sym sym]]
    MToken t -> [[]]
    _ -> []
,
 "to_iexports" -: process1__to_iexports msg = return $ case msg of
    MOutput_Rule n name syms -> [IOutput_XRule]
    MOutput_Start n -> [IOutput_XStart]
    MOutput_Sym sym -> [IOutput_XSym]
    MProductive sym -> [IProductive_Sym sym]
    MProductiveRule n name syms -> [IProductiveLeftXNameRights n]
    MRule n name syms -> [IXRule]
    MStart n -> [IXStart]
    MToken t -> [IXToken]
    _ -> []
,
 "to_pushouts" -: process1__to_pushouts i = return $ case i of
    _ -> []
,
 "to_outmsgs" -: process2__to_outmsgs im i2ex =
let items = M.toAscList i2ex
    keys = fmap fst items
    exs = fmap snd items
in  return $ case im of
  >- -> case keys of
    [] -> case exs of
      [] -> [MRule n name syms, MStart n, MToken t]
  MProductiveRule n name syms -> case keys of
    [] -> case exs of
      [] -> [MProductive sym]
  MReachable sym -> case keys of
    [] -> case exs of
      [] -> [MOutput_Sym sym, MReachableLeft n]
  MReachableLeft n -> case keys of
    [IProductiveLeftXNameRights n] -> case exs of
      [MProductiveRule n name syms] -> [MOutput_Rule n name syms, MReachable sym]
  MRule n name syms -> case keys of
    [] -> case exs of
      [] -> [MRuleTail n name syms sym:[sym1]]
  MRuleTail n name syms sym:[sym1] -> case keys of
    [] -> case exs of
      [] -> [MProductiveRule n name syms]
    [IProductive_Sym sym] -> case exs of
      [MProductive sym] -> [MRuleTail n name syms sym:[sym1]]
  MStart n -> case keys of
    [IProductive_Sym sym] -> case exs of
      [MProductive sym] -> [MOutput_Start n, MReachable sym]
  MToken t -> case keys of
    [] -> case exs of
      [] -> [MProductive sym]
)
