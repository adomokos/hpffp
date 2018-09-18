module Hpffp.OuterInnerSpec where

import Test.Hspec

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- We only need to use return once
-- because it's one big Monad
embedded :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded = return 1

-- We can peel away the layers one by one:
maybeUnwrap :: ExceptT String
               (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

-- Next
eitherUnwrap :: ReaderT () IO
                (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

-- Lastly
readerUnwrap :: ()
             -> IO (Either String
                           (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Lexically inner is structurally outer" $ do
        it "can work with monadic values" $ do
            result <- readerUnwrap ()
            result `shouldBe` Right (Just 1)
        it "can treat Reader/Maybe/Either stack as compososition" $ do
            let result = ((const . Right . Just $ 1) ()) :: Either String (Maybe Int)
            result `shouldBe` Right (Just 1)
