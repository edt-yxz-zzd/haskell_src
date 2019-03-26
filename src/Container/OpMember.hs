
module Container.OpMember
where
import Container.IContainer
{-
class (IContainer a) => OpMember a where
    member :: Element a -> a -> Bool
-}
class WithMemberType a => OpMember a where
    member :: MemberType a -> a -> Bool


