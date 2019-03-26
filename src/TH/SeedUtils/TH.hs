
module TH.SeedUtils.TH
where


class OpGetName a where
    getName :: a -> Maybe Name
instance OpGetName Name where
    getName = Just
instance OpGetName TyVarBndr where
    getName (PlainTV n) = Just n
    getName (KindedTV n _) = Just n
instance OpGetName Pat where
    getName (VarP n) = Just n
    getName (ConP n _) = Just n
    getName (InfixP _ n _) = Just n
    getName (UInfixP _ n _) = Just n
    getName (ParensP p) = getName p
    getName (TildeP p) = getName p
    getName (AsP n _) = Just n
    getName (RecP n _) = Just n
    getName (SigP p) = getName p
    getName _ = Nothing
instance OpGetName Type where
    getName (ForallT _ _ t) = getName t
    getName (AppT t _) = getName t
    getName (SigT t _) = getName t
    getName (VarT n) = Just n
    getName (ConT n) = Just n
    getName (PromotedT n) = Just n
    getName (InfixT _ n _) = Just n
    getName (UInfixT _ n _) = Just n
    getName (ParensT t) = getName t
instance OpGetName TypeFamilyHead where
    getName (TypeFamilyHead n _ _ _) = Just n
instance OpGetName Foreign where
    getName (ImportF _ _ _ n _) = Just n
    getName (ExportF _ _ n _) = Just n
instance OpGetName Pragma where
    getName (InlineP n _ _ _) = Just n
    getName (SpecialiseP n _ _ _) = Just n
    getName (SpecialiseInstP t) = getName t
    getName _ = Nothing
instance OpGetName Dec where
    getName (FunD n _) = Just n
    getName (ValD pat _) = getName pat
    getName (DataD _ n _ _ _ _) = Just n
    getName (NewtypeD _ n _ _ _ _) = Just n
    getName (TySynD n _ _) = Just n
    getName (ClassD _ n _ _ _) = Just n
    getName (InstanceD _ _ t _) = getName t
    getName (SigD n _) = Just n
    getName (ForeignD f) = getName f
    getName (InfixD _ n) = Just n
    getName (PragmaD p) = getName p
    getName (DataFamilyD n _ _) = Just n
    getName (DataInstD _ n _ _ _ _) = Just n
    getName (NewtypeInstD _ n _ _ _ _) = Just n
    getName (TySynInstD n _) = Just n
    getName (OpenTypeFamilyD tfh) = getName tfh
    getName (ClosedTypeFamilyD tfh) = getName tfh
    getName (RoleAnnotD n _) = Just n
    getName (StandaloneDerivD _ ty) = getName ty
    getName (DefaultSigD n _) = Just n
