



module Container.OpSingleton
where
import Container.IContainer

class IContainer a => OpSingleton a where
    singleton :: Element a -> a

