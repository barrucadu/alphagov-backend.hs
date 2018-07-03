{-# LANGUAGE LambdaCase #-}

-- | The example service.
module GDS.Example where

import           Control.Concurrent.STM      (STM)
import qualified Control.Concurrent.STM      as STM
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Data.Aeson                  (Value (..))
import qualified Data.HashMap.Strict         as M
import           Data.Semigroup              ((<>))
import           Data.UUID.Types             (UUID)
import qualified Data.UUID.V4                as UUID
import           Servant

import qualified GDS.API.Example             as GDS


-------------------------------------------------------------------------------
-- * API server

-- | The API server.
server :: Store -> Server GDS.API
server store =
  create store
    :<|> retrieve store
    :<|> delete store
    :<|> replace store
    :<|> patch store


-------------------------------------------------------------------------------
-- * Handlers

-- | Store a new record, and return the UUID.
create :: Store -> Value -> Handler UUID
create = insertIntoStore

-- | Get a record by UUID.
retrieve :: Store -> UUID -> Handler Value
retrieve store uuid = lookupInStore store uuid >>= \case
  Just value -> pure value
  Nothing    -> throwError err404

-- | Delete a record by UUID.
delete :: Store -> UUID -> Handler NoContent
delete store uuid = do
  deleteFromStore store uuid
  pure NoContent

-- | Replace a record by UUID.
replace :: Store -> UUID -> Value -> Handler NoContent
replace store uuid value = updateInStore store uuid (const value) >>= \case
  Just _  -> pure NoContent
  Nothing -> throwError err404

-- | Update a record by UUID, and return the new record.
--
-- The old and new values must be objects: if not, the new value
-- simply replaces the old.
patch :: Store -> UUID -> Value -> Handler Value
patch store uuid diff = updateInStore store uuid (merge diff) >>= \case
  Just new -> pure new
  Nothing  -> throwError err404


-------------------------------------------------------------------------------
-- * Store

-- | Store for JSON values.
type Store = TVar (M.HashMap UUID Value)

-- | Create a new, empty, store.
newStore :: MonadIO m => m Store
newStore = liftIO (newTVarIO M.empty)

-- | Insert a value into the store, producing a new UUID.
insertIntoStore :: MonadIO m => Store -> Value -> m UUID
insertIntoStore store value = do
  uuid <- liftIO UUID.nextRandom
  atomically (modifyTVar store (M.insert uuid value))
  pure uuid

-- | Update a value in the store.
updateInStore
  :: MonadIO m => Store -> UUID -> (Value -> Value) -> m (Maybe Value)
updateInStore store uuid f = atomically $ do
  old <- M.lookup uuid <$> readTVar store
  case old of
    Just orig -> do
      let new = f orig
      modifyTVar store $ M.insert uuid new
      pure (Just new)
    Nothing -> pure Nothing

-- | Look up a value in the store.
lookupInStore :: MonadIO m => Store -> UUID -> m (Maybe Value)
lookupInStore store uuid = atomically $ do
  s <- readTVar store
  pure (M.lookup uuid s)

-- | Remove a value from the store.
deleteFromStore :: MonadIO m => Store -> UUID -> m ()
deleteFromStore store = atomically . modifyTVar store . M.delete


-------------------------------------------------------------------------------
-- * Utilities

-- | 'STM.atomically' in any 'MonadIO'.
atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically

-- | Merge two objects.
merge
  :: Value -- ^ The diff.
  -> Value -- ^ The original value.
  -> Value
merge (Object diff) (Object orig) = Object (diff <> orig)
merge new           _             = new
