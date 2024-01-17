module ConfigReader
where


data Config = ConfC {upperCaseT :: Bool, reverseStrT :: Bool}
        -- El tipus 'Config' representa informacio global de l'aplicacio
        -- que es consultada des de diferents parts de l'aplicacio.

newtype ConfigReader a = R (Config -> a)

runReader :: ConfigReader a -> Config -> a
runReader (R r) = r

-- Functor, Applicative and Monad instances for type Reader

instance Functor ConfigReader where
    fmap f r = R $ \ e -> f (runReader r e)

instance Applicative ConfigReader where
    pure x = R $ \ e -> x

    rf <*> rx = R $ \ e -> runReader rf e (runReader rx e)

instance Monad ConfigReader where
    rx >>= k = R $ \ e ->
        let
           a = runReader rx e
           b = runReader (k a) e 
        in b
    
    -- error "A completar per l'estudiant"

-- Reader functions

-- Retrieves the monad environment.
ask :: ConfigReader Config
ask = asks id

-- Retrieves a function of the current environment.
asks :: (Config -> a) -> ConfigReader a
asks f = R f

-- Execute a computation in a modified environment.
withReader :: (Config -> Config) -> ConfigReader a -> ConfigReader a
withReader f r' = R $ runReader r' . f

