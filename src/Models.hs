{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Text
import Data.Time.Clock
import Control.Monad.IO.Class (MonadIO)

import Database.Persist.TH
import Snap.Snaplet.Auth.Backends.Persistent (authEntityDefs)

import qualified Database.Esqueleto as E

share [mkPersist sqlSettings, mkMigrate "migrateAll"] $ authEntityDefs ++ [persistLowerCase|
  BlogPost
    title String
    content String
    deriving Eq Show
|]

selectBlogPosts :: MonadIO m => E.SqlPersistT m [BlogPost]
selectBlogPosts = do
    posts <-
        E.select $
        E.from $ \blogPost -> do
            E.orderBy [E.asc (blogPost E.^. BlogPostTitle)]
            E.limit 3
            return blogPost
    return $ E.entityVal <$> posts
