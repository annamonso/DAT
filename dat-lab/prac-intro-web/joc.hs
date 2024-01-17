{-# LANGUAGE OverloadedStrings #-}

module Main
where
import           Network.Wai
import           Network.Wai.Handler.Warp (runEnv)

import           Handler_T2

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- ****************************************************************

main :: IO ()
main = do
    -- runEnv :: Port -> Application -> IO ()
    runEnv 4050 $ dispatchHandler gameApp

-- ****************************************************************
-- Controller

gameApp :: Handler HandlerResponse
gameApp = onMethod
    [ ("GET", doGet)
    , ("POST", doPost)
    ]

doGet :: Handler HandlerResponse
doGet = do
        mbSessionName <- getSession "playState"
        let gameState = case mbSessionName of
                       Just value -> value
                       Nothing  -> startState 
        respHtml $ htmlView gameState
        
doPost :: Handler HandlerResponse
doPost = do
        mbSessionName <- getSession "playState"
        let gameState = case mbSessionName of
                       Just value -> value
                       Nothing  -> startState
        
        mbvalue <- lookupPostParam "playText"
        
        let euserName = do -- Monad (Either T.Text)
        
                value <- maybe (Left "Caracter obligatori") Right mbvalue
                if T.null value then Left "Caracter obligatori"
                else Right value
                
        case euserName of
            Left err ->
                respHtml $ htmlView gameState
            Right userName -> do
                setSession "playState" (playText userName gameState)
                respRedirect "#"
     
        

-- ****************************************************************
-- View

htmlView :: GameState -> H.Html
htmlView game =
    H.docTypeHtml $ do
        H.head $
            H.title "A simple game ..."
        H.body $ do
            H.h1 $ H.text $ "Game state: " <> T.pack (show game)
            H.hr
            H.form H.! A.method "POST" H.! A.action "#" $ do
                H.p $ do
                    H.span "String to play:"
                    H.input H.! A.name "playText"
                H.input H.! A.type_ "submit" H.! A.name "ok" H.! A.value "Play"

-- ****************************************************************
-- Model

-- Tipus de l'estat
type GameState = (Bool, Int)

startState :: GameState
startState = (False, 0)

playChar :: Char -> GameState -> GameState
playChar '*' state = if fst(state) == True then (False, snd(state)) else (True, snd(state))
playChar '+' state = if fst(state) == True then (True,snd(state) + 1) else (False, snd(state))
playChar '-' state = if fst(state) == True then (True,snd(state) - 1) else (False, snd(state))
playChar _ state = state

playString :: String -> GameState -> GameState
playString [] state = state
playString (x:xs) state = playString xs (playChar x state)
                                        

playText :: Text -> GameState -> GameState
playText t = playString (T.unpack t)


