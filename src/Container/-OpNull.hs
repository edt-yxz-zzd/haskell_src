
module Container.OpNull
where
import Container.IContainer (IContainer)


class (IContainer a) => OpNull a where
    null :: a -> Bool


