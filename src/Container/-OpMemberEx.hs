
module Container.OpMember
where
import Container.IContainer (IContainer)


class (Container a, Boolean (MemberExResult a))
    => OpMember a where
    member :: Element a -> a -> Bool
    member e = explain . member_ex e
    type MemberExResult a :: *
    member_ex :: Element a -> a -> MemberExResult a
    default member_ex :: OpDynTheOnlyValue (MemberExResult a)
        => Element a -> a -> MemberExResult a
    member_ex _ _ = the_only_value

