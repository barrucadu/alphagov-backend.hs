{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module GDS.API.Example where

import           Data.Aeson      (Value)
import           Data.Proxy      (Proxy (..))
import           Data.UUID.Types (UUID)
import           Servant.API
import           Servant.Client  (ClientM, client)


-------------------------------------------------------------------------------
-- * API definition

-- | The example service: a simple store of arbitrary JSON indexed by
-- UUID.
type API =
  (ReqBody '[JSON] Value :> PostCreated '[JSON] UUID)
  :<|> (Capture "uuid" UUID :> Get '[JSON] Value)
  :<|> (Capture "uuid" UUID :> DeleteNoContent '[JSON] NoContent)
  :<|> (Capture "uuid" UUID :> ReqBody '[JSON] Value :> Put '[JSON] NoContent)
  :<|> (Capture "uuid" UUID :> ReqBody '[JSON] Value :> Patch '[JSON] Value)

-- | Boilerplate.
api :: Proxy API
api = Proxy

-------------------------------------------------------------------------------
-- * Client

-- | Store a new record, and return the UUID.
create :: Value -> ClientM UUID

-- | Get a record by UUID.
retrieve :: UUID -> ClientM Value

-- | Delete a record by UUID.
delete :: UUID -> ClientM NoContent

-- | Replace a record by UUID.
replace :: UUID -> Value -> ClientM NoContent

-- | Update a record by UUID, and return the new record.
--
-- The old and new values must be objects: if not, the new value
-- simply replaces the old.
patch :: UUID -> Value -> ClientM Value

create :<|> retrieve :<|> delete :<|> replace :<|> patch = client api
