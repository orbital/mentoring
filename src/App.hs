{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module App where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Base (MonadBase)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Servant (ServantErr)




-----------------------------------------------------------------------

newtype App a = App {
  unApp :: ExceptT ServantErr IO a
} deriving (Monad, Functor, Applicative, MonadIO, MonadCatch, MonadThrow, MonadBase IO, MonadError ServantErr)


runApp :: App a -> ExceptT ServantErr IO a
runApp action =
    unApp action


instance MonadBaseControl IO App where
  type StM App a = StM (ExceptT ServantErr IO) a
  liftBaseWith f = App $ liftBaseWith $ \q -> f (q . unApp)
  restoreM = App . restoreM
