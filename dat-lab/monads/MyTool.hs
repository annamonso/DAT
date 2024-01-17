module MyTool where

import Control.Applicative
import Data.Char

data MyTool a = MyToolC {runMyTool :: a }  --un monads es una accio que quan s'exacuta genera un valor

instance( Show a ) => Show( MyTool a ) where
    show( MyToolC a ) = show a

instance Functor MyTool where    --fmap :: (a -> b) ->f a -> fb
    fmap f (MyToolC a ) = MyToolC(f a)
    
    
instance Applicative MyTool where
    pure x = MyToolC x -- crea una accio pura
    
    MyToolC g <*> mt = fmap g mt  -- <*> f (a -> b) -> fa -> fb
    
instance Monad MyTool where
    return = pure --return  x = MyToolC x
    MyToolC x >>= f = f x -- encadenar accions   >>= :: m a -> (a -> m b) -> mb
                                                                 
einaMajuscules :: String -> MyTool String  --  (a -> m b)--------|
einaMajuscules str = MyToolC( fmap toUpper str )  --toUpper::Char -> Char 

einaReverse :: String -> MyTool String 
einaReverse str = MyToolC( reverse str )


einaRemChar :: Char -> String -> MyTool String
einaRemChar _ [] = pure ""  
einaRemChar r (x:xs) = do
                          rest <- einaRemChar r xs
                          if r == x
                            then pure rest
                            else pure (x : rest)
                            
einaRemSpace :: String -> MyTool String
einaRemSpace [] = pure ""
einaRemSpace (x:xs) = 
                     do
                       res <- einaRemSpace xs
                       if x == ' '
                       then pure res
                       else pure (x:res)   

charMaj :: Char -> Char
charMaj r = toUpper r

eina :: Char -> String -> MyTool String
eina r str =  
    einaMajuscules str >>= \strMaj ->
        einaReverse strMaj >>= \strMajRev ->
          einaRemChar (charMaj r) strMajRev >>= \strRemMajRev ->
            einaRemSpace strRemMajRev >>= \strRemSMajRev ->
              pure strRemSMajRev
            
    
        
