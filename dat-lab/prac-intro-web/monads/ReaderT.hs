
module ReaderT
where
import Control.Monad.Trans
import Data.Functor.Identity


newtype ReaderT e m a = R (e -> m a)

-- Runs a ReaderT and extracts the final value from it.
runReaderT :: ReaderT e m a -> e -> m a
runReaderT (R r) = r

-- Functor, Applicative and Monad instances for type Reader

instance Monad m => Functor (ReaderT e m) where
    fmap f r = R $ \e -> do
                x <- (runReaderT r) e
                pure(f x)
    
    --error "A completar per l'estudiant" ho tinc malament

instance Monad m => Applicative (ReaderT e m) where
    pure x = R $ \ _ -> pure x

    rf <*> rx = R $ \ e -> runReaderT rf e <*> runReaderT rx e

instance Monad m => Monad (ReaderT e m) where
    rx >>= k = R $ \e ->
                    do
                       a <- runReaderT rx e
                       res <- runReaderT (k a) e
                       pure(res)
    
    --error "A completar per l'estudiant"

instance MonadTrans (ReaderT e) where
    lift m = R $ \e -> do
                    x<-m
                    pure(x)
    
    --error "A completar per l'estudiant"

-- Reader functions

-- Retrieves the monad environment.
ask :: Applicative m => ReaderT e m e
ask = asks id

-- Retrieves a function of the current environment.
asks :: Applicative m => (e -> a) -> ReaderT e m a
asks f = R $ pure . f

-- Execute a computation in a modified environment.
withReaderT :: (e -> e') -> ReaderT e' m a -> ReaderT e m a
withReaderT f r' = R $ runReaderT r' . f

-------------------------------------------------------------------

type Reader e = ReaderT e Identity

-- Runs a Reader and extracts the final value from it.
runReader :: Reader e a -> e -> a
runReader m e = runIdentity $ runReaderT m e

-- Execute a computation in a modified environment (a specialization of withReaderT).
withReader :: (e -> e') -> Reader e' a -> Reader e a
withReader = withReaderT

