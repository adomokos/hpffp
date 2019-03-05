module CH21.TraverseSpec where

import Test.Hspec

{-
  traverse :: (Applicative f, Traversable t)
           => (a -> f b) -> t a -> f (t b)

  -- It has similarities of fmap and (=<<) (flip-bind):

  fmap     :: (a -> b)   -> f a -> f b
  (=<<)    :: (a -> m b) -> m a -> m b
  traverse :: (a -> f b) -> t a -> f (t b)

  -- traverse is `fmap` composed with `sequenceA`:
  traverse f = sequenceA . fmap f

  -- mapM is traverse
  mapM :: Monad m
       => (a -> m b) -> [a] -> m [b]
  -- contrast this with:
  traverse :: Applicative f, Traversable t
           => (a -> f b) -> t a -> f (t b)

  -- So what's Traversable good for?
  -- Anytime you need to flip two type constructors around,
  -- or map something and flip them around, that' probably
  -- Traversable.
  sequence :: Applicative f
           => t (f a) -> f (t a)
  traverse :: Applicative f
           => (a -> f b) -> t a -> f (t b)
-}

-- simulate wreq
urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5" ]

get :: String -> IO String
get url = pure $ "The url invoked: " ++ url

mappingGet :: [IO String]
mappingGet = map get urls

main :: IO ()
main = hspec spec

traversedUrls :: IO [String]
traversedUrls = traverse get urls

spec :: Spec
spec =
  describe "#traverse" $ do
    it "has similarity to fmap and flip-bind" $ do
      fmap Just [1,2,3] `shouldBe` [Just 1, Just 2, Just 3]
      (sequenceA $ fmap Just [1,2,3])
        `shouldBe` Just [1,2,3]
      (sequenceA . fmap Just $ [1,2,3])
        `shouldBe` Just [1,2,3]
      traverse Just [1,2,3] `shouldBe` Just [1,2,3]
    it "works with a wreq example - simplified here" $ do
      let [result1, result2] = mappingGet
      result1 `shouldReturn` "The url invoked: http://httpbin.org/ip"
      result2 `shouldReturn` "The url invoked: http://httpbin.org/bytes/5"
      let result = traversedUrls
      result `shouldReturn` [ "The url invoked: http://httpbin.org/ip"
                            , "The url invoked: http://httpbin.org/bytes/5" ]
