
import ReaderT
import Data.Char
import Data.List
import Control.Monad.Trans (lift)
import Control.Monad (when)


data Config = ConfC {upperCaseT :: Bool, reverseStrT :: Bool}
        -- El tipus 'Config' representa informacio global de l'aplicacio
        -- que es consultada des de diferents parts de l'aplicacio.

caseTool :: String -> ReaderT Config IO String
caseTool str = do
    cnf <- asks upperCaseT
    if cnf then return (fmap toUpper str)
    else return (fmap toLower str)

reverseTool :: String -> ReaderT Config IO String
reverseTool str = do
            cnf <- asks reverseStrT
            if cnf then return (reverse str)
            else return (str)

caseRevTool :: String -> ReaderT Config IO String
caseRevTool str = reverseTool str >>= \x -> caseTool x

lengthTool :: String -> ReaderT Config IO Int
lengthTool str = return(length str)

caseRevEvenTool :: String -> ReaderT Config IO String
caseRevEvenTool str = do
                        len <- lengthTool str
                        if even len then 
                             return (reverse(fmap toUpper str))
                        else return str

loop :: ReaderT Config IO ()
loop = do  -- Monad 'ReaderT Config IO'
    -- La configuracio esta en el context del monad
    test <- lift $ do
        putStr "Entra línia: "
        getLine
    when (not $ null test) $ do
        z <- app test -- Quin es el tipus de z?
        -- mostrar per pantalla z
        --error "A completar per l'estudiant"
        lift $ putStrLn z
        loop

app :: String -> ReaderT Config IO String
app test = do
    x <- caseRevEvenTool test
    y <- withReaderT (\c -> ConfC{ upperCaseT = not (upperCaseT c), reverseStrT = not (reverseStrT c) }) $
        caseRevTool x
    reverseTool (x <> y)

confInit :: Config
confInit = ConfC {upperCaseT = True, reverseStrT = False}

main :: IO ()
main =
    runReaderT loop confInit

