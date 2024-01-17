
{-# LANGUAGE OverloadedStrings #-}

module Forums.Handler
where
import Forums.View
import Forums.Found
import Forums.Model
import Control.Monad.IO.Class (liftIO)


import Develop.DatFw
import Develop.DatFw.Form
import Develop.DatFw.Form.Fields

import Data.Text as T

-- ---------------------------------------------------------------

markdownField :: Field (HandlerFor ForumsApp) Markdown
markdownField = checkMap
        (\ t -> if T.length t < minPostLen then Left "Text massa curt"
                else if T.length t > maxPostLen then Left "Text massa llarg"
                else Right (Markdown t))
        getMdText
        textareaField 

-- ---------------------------------------------------------------
-- Controller handlers: Home

newForumForm :: AForm (HandlerFor ForumsApp) NewForum
newForumForm =
    NewForum <$> freq textField (withPlaceholder "Introduiu el títol del fòrum" "Titol") Nothing
             <*> freq markdownField (withPlaceholder "Introduiu la descripció del fòrum" "Descripció") Nothing        
       
newTopicForm :: AForm (HandlerFor ForumsApp) NewTopic
newTopicForm =
    NewTopic <$> freq textField (withPlaceholder "Introduiu el títol del tòpic" "Titol") Nothing
             <*> freq markdownField (withPlaceholder "Introduiu la descripció del tòpic" "Descripció") Nothing
            
newReplyForm :: AForm (HandlerFor ForumsApp) Markdown
newReplyForm = freq markdownField (withPlaceholder "Introduiu una resposta" "Resposta") Nothing
             

getHomeR :: HandlerFor ForumsApp Html
getHomeR = do
    -- Get authenticated user
    mbuser <- maybeAuth
    -- Get a fresh form
    fformw <- generateAFormPost newForumForm
    -- Return HTML content
    appLayout $ homeView mbuser fformw

postHomeR :: HandlerFor ForumsApp Html
postHomeR = do
    user <- requireAuth
    (fformr, fformw) <- runAFormPost newForumForm
    case fformr of
        FormSuccess newtheme -> do
            runDbAction $ addForum (fst user) newtheme
            redirect HomeR
        _ ->
            appLayout $ homeView (Just user) fformw

-- ---------------------------------------------------------------
-- Controller handlers: Forum

getForumR :: ForumId -> HandlerFor ForumsApp Html
getForumR fid = do
    -- Get requested forum from data-base.
    -- Short-circuit (responds immediately) with a 'Not found' status if forum don't exist
    forum <- runDbAction (getForum fid) >>= maybe notFound pure
    mbuser <- maybeAuth
    -- Other processing (forms, ...)
    -- ... A completar per l'estudiant
    -- Return HTML content
    tformw <- generateAFormPost newTopicForm
    appLayout $ forumView mbuser (fid, forum) tformw
    
getEditForumR :: ForumId -> HandlerFor ForumsApp Html 
getEditForumR fid = do
    -- Get requested forum from data-base.
    -- Short-circuit (responds immediately) with a 'Not found' status if forum don't exist
    forum <- runDbAction (getForum fid) >>= maybe notFound pure
    mbuser <- maybeAuth
    userId <- requireAuthId
    -- Other processing (forms, ...)
    -- ... A completar per l'estudiant
    -- Return HTML content
    let moderatorId = fdModeratorId forum
    tformw <- generateAFormPost newTopicForm
    
    if (userId == moderatorId) then
        appLayout $ forumView mbuser (fid, forum) tformw
    else
        appLayout $ forumView Nothing (fid, forum) tformw

postForumR :: ForumId -> HandlerFor ForumsApp Html -- EN PROCES
postForumR fid = do
    user <- requireAuth
    form <- runDbAction (getForum fid) >>= maybe notFound pure
    (tformr, tformw) <- runAFormPost newTopicForm
    case tformr of
        FormSuccess newtopic -> do
            runDbAction $ addTopic fid (fst user) newtopic
            redirect (ForumR fid)
        _ -> do
            appLayout $ forumView (Just user) (fid, form) tformw

postEditForumR :: ForumId -> HandlerFor ForumsApp Html -- EN PROCES
postEditForumR fid = do
    user <- requireAuth
    userId <- requireAuthId
    
    forum <- runDbAction (getForum fid) >>= maybe notFound pure
    (fformr, fformw) <- runAFormPost newForumForm
    let moderatorId = fdModeratorId forum
    if (userId == moderatorId) 
        then do
            case fformr of
                FormSuccess newforum -> do
                    runDbAction $ editForum fid (nfTitle newforum) (nfDescription newforum)
                    liftIO $ putStrLn $ "New Forum: "
                    redirect (ForumR fid)
                _ -> do
                    appLayout $ forumView (Just user) (fid, forum) fformw          
        else do
             setMessage "Error d'autenticaciò"
             liftIO $ putStrLn $ " Error"
             redirect (ForumR fid)
             
postDeleteForumR :: ForumId -> HandlerFor ForumsApp Html -- EN PROCES
postDeleteForumR fid = do
    user <- requireAuth
    userId <- requireAuthId
    
    forum <- runDbAction (getForum fid) >>= maybe notFound pure
    let moderatorId = fdModeratorId forum
    if (userId == moderatorId) 
        then do
            runDbAction $ deleteForum fid 
            liftIO $ putStrLn $ "Forum deleted"
            redirect HomeR     
        else do
             setMessage "Error d'autenticaciò"
             liftIO $ putStrLn $ " Error"
             redirect (ForumR fid)
         
   
         

            
-- ---------------------------------------------------------------
-- Controller handlers: Topic

getTopicR :: TopicId -> HandlerFor ForumsApp Html
getTopicR tid = do
    -- Get requested forum from data-base.
    -- Short-circuit (responds immediately) with a 'Not found' status if forum don't exist
    topic <- runDbAction (getTopic tid) >>= maybe notFound pure
    mbuser <- maybeAuth
    -- Other processing (forms, ...)
    -- ... A completar per l'estudiant
    -- Return HTML content
    rformw <- generateAFormPost newReplyForm
    appLayout $ topicView mbuser (tid, topic) rformw

postTopicR :: TopicId -> HandlerFor ForumsApp Html
postTopicR tid = do
    user <- requireAuth 
    topic <- runDbAction (getTopic tid) >>= maybe notFound pure
    (rformr, rformw) <- runAFormPost newReplyForm
    case rformr of
        FormSuccess newPost -> do
            runDbAction $ addReply (tdForumId topic) tid (fst user) newPost
            redirect (TopicR tid)
        _ -> do
            appLayout $ topicView (Just user) (tid, topic) rformw
            
postDeleteTopicR :: TopicId -> HandlerFor ForumsApp Html
postDeleteTopicR tid = do
    user <- requireAuth
    userId <- requireAuthId
    
    topic <- runDbAction (getTopic tid) >>= maybe notFound pure
    let fid = tdForumId topic
    forum <- runDbAction (getForum fid) >>= maybe notFound pure
    let moderatorId = fdModeratorId forum
    
    if (userId == moderatorId) 
        then do
            runDbAction $ deleteTopic fid tid 
            liftIO $ putStrLn $ "Topic deleted"
            redirect (ForumR fid)
        else do
             setMessage "Error d'autenticaciò"
             liftIO $ putStrLn $ " Error"
             redirect (ForumR fid)
             
getReplyR :: PostId -> HandlerFor ForumsApp Html
getReplyR pid = do
    -- Get requested forum from data-base.
    -- Short-circuit (responds immediately) with a 'Not found' status if forum don't exist
    post <- runDbAction (getPost pid) >>= maybe notFound pure
    let tid = pdTopicId post
    topic <- runDbAction (getTopic tid) >>= maybe notFound pure
    mbuser <- maybeAuth
    -- Other processing (forms, ...)
    -- ... A completar per l'estudiant
    -- Return HTML content
    rformw <- generateAFormPost newReplyForm
    appLayout $ topicView mbuser (tid, topic) rformw
postDeleteReplyR :: PostId -> HandlerFor ForumsApp Html
postDeleteReplyR pid = do
    user <- requireAuth
    userId <- requireAuthId
    
    post <- runDbAction (getPost pid) >>= maybe notFound pure
    let tid = pdTopicId post
    topic <- runDbAction (getTopic tid) >>= maybe notFound pure
    let fid = tdForumId topic
    forum <- runDbAction (getForum fid) >>= maybe notFound pure
    let moderatorId = fdModeratorId forum
      
    
    if (userId == moderatorId) 
        then do
            runDbAction $ deletePost fid tid pid 
            liftIO $ putStrLn $ "Post deleted"
            redirect (TopicR tid)
        else do
             setMessage "Error d'autenticaciò"
             liftIO $ putStrLn $ " Error"
             redirect (TopicR tid)


-- ---------------------------------------------------------------
-- Controller handlers: Autenticació

loginForm :: MonadHandler m => AForm m (Text, Text)
loginForm =
    (,) <$> freq textField "Nom d'usuari" Nothing
        <*> freq passwordField "Clau d'accés" Nothing

getLoginR :: HandlerFor ForumsApp Html
getLoginR = do
    setUltDestReferer
    -- Return HTML page
    (_, formw) <- runAFormPost loginForm
    appLayout $ loginView formw

postLoginR :: HandlerFor ForumsApp Html
postLoginR = do
    (formr, formw) <- runAFormPost loginForm
    case formr of
        FormSuccess (name, password) -> do
            ok <- validatePassword name password
            if ok then do
                -- Good credentials
                Just uid <- runDbAction $ loginUser name
                setSession authId_SESSION_KEY $ toPathPiece uid
                redirectUltDest HomeR
            else do
                -- Login error
                setMessage "Error d'autenticaciò"
                redirect LoginR
        _ ->
            appLayout (loginView formw)
    where
        validatePassword :: Text -> Text -> HandlerFor ForumsApp Bool
        validatePassword name password = do
            mbuser <- runDbAction $ getUserByName name
            case mbuser of
                Nothing -> pure False
                Just (_, user) -> pure $ pHashValidate password $ udPassword user

handleLogoutR :: HandlerFor ForumsApp ()
handleLogoutR = do
    -- | After logout (from the browser), redirect to the referring page.
    setUltDestReferer
    deleteSession authId_SESSION_KEY
    redirectUltDest HomeR

