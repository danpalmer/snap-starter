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

import Database.Persist.TH
import Snap.Snaplet.Auth.Backends.Persistent (authEntityDefs)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] $ authEntityDefs ++ [persistLowerCase|
  BlogPost
    title String
    content String
    deriving Eq Show
|]
