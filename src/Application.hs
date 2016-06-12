{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.Persistent

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _db :: Snaplet PersistState
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasPersistPool (Handler a App) where
    getPersistPool = with db getPersistPool

instance HasPersistPool (Handler App (AuthManager App)) where
    getPersistPool = withTop db getPersistPool


------------------------------------------------------------------------------
type AppHandler = Handler App App


