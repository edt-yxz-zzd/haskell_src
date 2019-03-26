module Container.OpAnyElem
where
import Container.IContainer
import Container.OpEmpty

class (OpIsEmpty a, IContainer a) => OpAnyElem a where
    any_elem :: a -> Maybe (Element a)

