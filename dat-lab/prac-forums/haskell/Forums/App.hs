
{-# LANGUAGE OverloadedStrings #-}

module Forums.App
where
import Forums.Config
import Forums.Found
import Forums.Model
import Forums.Handler

import Develop.DatFw
import Routing

import Network.Wai

-- ---------------------------------------------------------------
-- Application initialization

makeApp :: IO Application
makeApp = do
    -- Open the database (the model state)
    db <- openDb forumsDbName
    toApp ForumsApp{ forumsDb = db }

-- ---------------------------------------------------------------
-- Main controller

instance Dispatch ForumsApp where
  dispatch = dispatchSite handle
    where
      handle route =
        case route of 
            HomeR -> onMethods
                [ whenMethod "GET" getHomeR
                , whenMethod "POST" postHomeR
                ]
            ForumR fid -> onMethods
                [ whenMethod "GET" $ getForumR fid
                , whenMethod "POST" $ postForumR fid
                ]
            EditForumR fid -> onMethods
                [ whenMethod "GET" $ getEditForumR fid
                , whenMethod "POST" $ postEditForumR fid
                ]
            DeleteForumR fid -> onMethods
                [ whenMethod "GET" $ getForumR fid
                , whenMethod "POST" $ postDeleteForumR fid
                ]
            TopicR tid -> onMethods
                [ whenMethod "GET" $ getTopicR tid
                , whenMethod "POST" $ postTopicR tid
                ]
            DeleteTopicR tid -> onMethods
                [ whenMethod "GET" $ getTopicR tid
                , whenMethod "POST" $ postDeleteTopicR tid
                ]
            DeleteReplyR rid -> onMethods
                [ whenMethod "GET" $ getReplyR rid
                , whenMethod "POST" $ postDeleteReplyR rid
                ]
            LoginR -> onMethods
                [ whenMethod "GET" getLoginR
                , whenMethod "POST" postLoginR
                ]
            LogoutR -> onMethods
                [ whenAnyMethod handleLogoutR
                ]

