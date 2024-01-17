
import Reader
import Data.Char
import Data.List


data Config = ConfC {upperCaseT :: Bool, reverseStrT :: Bool}
        -- El tipus 'Config' representa informacio global de l'aplicacio
        -- que es consultada des de diferents parts de l'aplicacio.

caseTool :: String -> Reader Config String
caseTool str = do
    cnf <- asks upperCaseT
    if cnf then return (fmap toUpper str)
    else return (fmap toLower str)

reverseTool :: String -> Reader Config String
reverseTool str = do
            cnf <- asks reverseStrT
            if cnf then return (reverse str)
            else return (str)
            
caseRevTool :: String -> Reader Config String
caseRevTool str = reverseTool str >>= \x -> caseTool x

lengthTool :: String -> Reader Config Int
lengthTool str = return(length str)

caseRevEvenTool :: String -> Reader Config String
caseRevEvenTool str = do
                        len <- lengthTool str
                        if even len then 
                             return (reverse(fmap toUpper str))
                        else return str

app :: Reader Config String
app = do
    let test = "Text de prova"
    x <- caseRevEvenTool test
    y <- withReader (\c -> ConfC{ upperCaseT = not (upperCaseT c), reverseStrT = not (reverseStrT c) }) $
        caseRevTool x
    reverseTool (x <> y)

confInit :: Config
confInit = ConfC {upperCaseT = False, reverseStrT = False}

main :: IO ()
main = do
    let res = runReader app confInit
    putStrLn res

