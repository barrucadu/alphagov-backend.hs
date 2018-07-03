{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson                (Value (..), object, toJSON)
import qualified Data.Aeson                as A
import           Data.String
import qualified Network.HTTP.Client       as HTTP
import           Network.HTTP.Types.Status (statusCode)
import           Servant.Client
import           Test.Hspec

import qualified GDS.API.Example           as GDS
import           GDS.Example               (merge)

main :: IO ()
main = do
  man <- HTTP.newManager HTTP.defaultManagerSettings
  let env = mkClientEnv man (BaseUrl Http "localhost" 3000 "")
  let r :: MonadIO m => ClientM a -> m a
      r ma = liftIO (either throwM pure =<< runClientM ma env)

  hspec $ do
    createSpec r
    retrieveSpec r
    deleteSpec r
    replaceSpec r
    patchSpec r

createSpec, retrieveSpec, deleteSpec, replaceSpec, patchSpec
  :: (forall a . ClientM a -> IO a) -> Spec

createSpec r = describe "create" $ do
  it "gives a UUID" $ do
    _ <- r (GDS.create expected)
    pure ()

  it "gives different UUIDs when creating multiple things" $ do
    uuid1 <- r (GDS.create expected)
    uuid2 <- r (GDS.create expected)
    uuid1 `shouldNotBe` uuid2
  where expected = object ["hello" .= "world"]

retrieveSpec r = describe "retrieve" $ do
  it "gives back the record" $ do
    uuid <- r (GDS.create expected1)
    val  <- r (GDS.retrieve uuid)
    val `shouldBe` expected1

  it "handles multiple records" $ do
    uuid1 <- r (GDS.create expected1)
    uuid2 <- r (GDS.create expected2)
    val1  <- r (GDS.retrieve uuid1)
    val2  <- r (GDS.retrieve uuid2)
    val1 `shouldBe` expected1
    val2 `shouldBe` expected2
 where
  expected1 = object ["hello" .= "world"]
  expected2 = object ["foo" .= "bar", "baz" .= (42 :: Int), "bat" .= Null]


deleteSpec r = describe "delete" $ do
  it "makes the record inaccessible" $ do
    uuid <- r (GDS.create expected1)
    _    <- r (GDS.delete uuid)
    r (GDS.retrieve uuid) `shouldThrow` throwsStatus 404

  it "doesn't throw an error if the record doesn't exist" $ do
    uuid <- r (GDS.create expected1)
    _    <- r (GDS.delete uuid)
    _    <- r (GDS.delete uuid)
    pure ()

  it "only deletes the given record" $ do
    uuid1 <- r (GDS.create expected1)
    uuid2 <- r (GDS.create expected2)
    _     <- r (GDS.delete uuid1)
    r (GDS.retrieve uuid1) `shouldThrow` throwsStatus 404
    val2 <- r (GDS.retrieve uuid2)
    val2 `shouldBe` expected2
 where
  expected1 = object ["hello" .= "world"]
  expected2 = object ["foo" .= "bar", "baz" .= (42 :: Int), "bat" .= Null]


replaceSpec r = describe "replace" $ do
  it "replaces the record" $ do
    uuid <- r (GDS.create expected1)
    _    <- r (GDS.replace uuid expected2)
    val' <- r (GDS.retrieve uuid)
    val' `shouldBe` expected2

  it "throws a 404 if the record doesn't exist" $ do
    uuid <- r (GDS.create expected1)
    _    <- r (GDS.delete uuid)
    r (GDS.replace uuid expected2) `shouldThrow` throwsStatus 404

  it "only replaces the given record" $ do
    uuid1 <- r (GDS.create expected1)
    uuid2 <- r (GDS.create expected2)
    _     <- r (GDS.replace uuid1 expected3)
    val1' <- r (GDS.retrieve uuid1)
    val2' <- r (GDS.retrieve uuid2)
    val1' `shouldBe` expected3
    val2' `shouldBe` expected2
 where
  expected1 = object ["hello" .= "world"]
  expected2 = object ["foo" .= "bar", "baz" .= (42 :: Int), "bat" .= Null]
  expected3 = toJSON [expected1, expected2]

patchSpec r = describe "patch" $ do
  it "replaces the record if the old isn't an object" $ do
    uuid <- r (GDS.create nonobject)
    val' <- r (GDS.patch uuid object1)
    val' `shouldBe` object1

  it "replaces the record if the new isn't an object" $ do
    uuid <- r (GDS.create object1)
    val' <- r (GDS.patch uuid nonobject)
    val' `shouldBe` nonobject

  it "merges the records if both are objects" $ do
    uuid <- r (GDS.create object1)
    val' <- r (GDS.patch uuid object2)
    val' `shouldBe` merge object2 object1

  it "saves the patched value" $ do
    uuid <- r (GDS.create object1)
    val1 <- r (GDS.patch uuid object2)
    val2 <- r (GDS.retrieve uuid)
    val2 `shouldBe` val1

  it "throws a 404 if the record doesn't exist" $ do
    uuid <- r (GDS.create object1)
    _    <- r (GDS.delete uuid)
    r (GDS.patch uuid object2) `shouldThrow` throwsStatus 404

  it "only patches the given record" $ do
    uuid1 <- r (GDS.create object1)
    uuid2 <- r (GDS.create object2)
    val1' <- r (GDS.patch uuid1 object2)
    val2' <- r (GDS.retrieve uuid2)
    val1' `shouldBe` merge object2 object1
    val2' `shouldBe` object2
 where
  object1   = object ["hello" .= "world"]
  object2   = object ["foo" .= "bar", "baz" .= (42 :: Int), "bat" .= Null]
  nonobject = toJSON [object1, toJSON (42 :: Int)]


-------------------------------------------------------------------------------
-- * Utils

-- | Check a 'ServantError' matches a given status code.
throwsStatus :: Int -> ServantError -> Bool
throwsStatus code (FailureResponse resp) =
  statusCode (responseStatusCode resp) == code
throwsStatus _ _ = False

-- | Construct a key/value pair
(.=) :: (A.ToJSON v, A.KeyValue kv) => String -> v -> kv
k .= v = fromString k A..= v
