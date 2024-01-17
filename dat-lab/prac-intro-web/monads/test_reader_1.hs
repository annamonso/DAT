
import ConfigReader
import Data.Char
import Data.List


caseTool :: String -> ConfigReader String
caseTool str = do
    cnf <- asks upperCaseT
    if cnf then return (fmap toUpper str)
    else return (fmap toLower str)

{--
Implementar la funcio monadica reverseTool, que
depenent de la configuracio
inverteix l'ordre dels caracters d'un String
--}
reverseTool :: String -> ConfigReader String
reverseTool str = do
    cnf <- asks reverseStrT
    if cnf then return (reverse str)
    else return (str)

{--
Implementar la funcio monadica caseRevTool, que
depenent de la configuracio
inverteix l'ordre i converteix a majuscules els caracters d'un String
--}
caseRevTool :: String -> ConfigReader String
caseRevTool str = reverseTool str >>= \x -> caseTool x

{--
Implementar la funcio monadica lengthTool dona la
longitud d'un String
--}
lengthTool :: String -> ConfigReader Int
lengthTool str = return(length str)

{--
Implementar la funcio monadica caseRevEvenTool, que
depenent de la configuracio
inverteix l'ordre i converteix a majuscules els caracters d'un String
només si la longitud de l'String es parell
--}
caseRevEvenTool :: String -> ConfigReader String
caseRevEvenTool str = do
                        len <- lengthTool str
                        if even len then 
                             return (reverse(fmap toUpper str))
                        else return str

app :: ConfigReader String
app = do
    let test = "Text de prova "
    x <- caseRevEvenTool test
    y <- withReader (\c -> ConfC{ upperCaseT = not (upperCaseT c), reverseStrT = not (reverseStrT c) }) $
        caseRevTool x
    reverseTool (x <> y)

confInit :: Config
confInit = ConfC {upperCaseT = True, reverseStrT = False}

main :: IO ()
main = do
    let res = runReader app confInit
    putStrLn res

