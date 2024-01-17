
module Reader
where


newtype Reader e a = R (e -> a)

runReader :: Reader e a -> e -> a
runReader (R r) = r

-- Functor, Applicative and Monad instances for type Reader

instance Functor (Reader e) where
    fmap f r = R $ \e -> f(runReader r e)
    
    --error "A completar per l'estudiant"

instance Applicative (Reader e) where
    pure x = R $ \e -> x --error "A completar per l'estudiant"
    rf <*> rx = R $ \e -> runReader rf e (runReader rx e)    --error --"A completar per l'estudiant"

instance Monad (Reader e) where
    rx >>= k = R $ \e ->
                    let
                        a = runReader rx e
                        res = runReader (k a) e
                    in res
    
    --error "A completar per l'estudiant"

-- Reader functions

-- Retrieves the monad environment.
ask :: Reader e e
ask = asks id

-- Retrieves a function of the current environment.
asks :: (e -> a) -> Reader e a
asks f = R f

-- Execute a computation in a modified environment.
withReader :: (e -> e') -> Reader e' a -> Reader e a
withReader f r' = R $ runReader r' . f

