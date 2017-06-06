{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Examples.Errors where

import App (App)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (Except, MonadError)
import Data.Text (Text)
import Servant hiding (Application)


type LeaseId = Text
type ApplicationId = Text
data Lease = Lease { applicationId :: Text }
data Application = Application {}
data LeaseAgreement = LeaseAgreement {}



type API =
    Capture "lease-id" LeaseId :> Get '[JSON] LeaseAgreement


server :: ServerT API App
server = getAgreement
  where
    getAgreement i = do
      -- how can I run this? My `App` already has a different MonadError in it.
      _ $ getLeaseAgreement i




-- It would be nice to use the Maybe or Except monad in `getLeaseAgreement`. They express my intent very well: do something if you can, or exit early with an error (or nothing).
-- But I don't want this function to worry about throwing a ServantErr.
-- I want it to have its own error type, or a Maybe

data LeaseError
    = LeaseMissing LeaseId
    | AppMissing ApplicationId


getLeaseAgreement :: (MonadIO m, MonadError LeaseError m) => LeaseId -> m LeaseAgreement
getLeaseAgreement i = do
    l <- ifMissing (LeaseMissing i) $ leaseFind i
    let aid = applicationId l
    a <- ifMissing (AppMissing aid) $ appFind aid
    leaseAgreement a l


ifMissing :: MonadError LeaseError m => LeaseError -> m (Maybe a) -> m a
ifMissing e action = do
    mv <- action
    case mv of
      Nothing -> throwError e
      Just v -> pure v



-- these functions correspond to external services

leaseFind :: MonadIO m => LeaseId -> m (Maybe Lease)
leaseFind = undefined


appFind :: MonadIO m => ApplicationId -> m (Maybe Application)
appFind = undefined


leaseAgreement :: MonadIO m => Application -> Lease -> m LeaseAgreement
leaseAgreement = undefined
