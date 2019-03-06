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

{-
-- Example from Haskell Wikibook:
-- https://en.wikibooks.org/wiki/Haskell/Traversable

instance Functor [] where
  fmap _ [] = []
  fmap f (x:xs) = f x : fmap f xs

instance Foldable [] where
  foldMap _ [] = []
  foldMap f (x:xs) = f x <> foldMap f xs

instance Traversable [] where
  sequenceA [] = pure []
  sequenceA (u:us) = (:) <$> u <*> sequenceA us

-- Traversable is to Applicative contexts what Foldable is to Monoid values.
-}

deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
deleteIfNegative x = if x < 0 then Nothing else Just x

rejectWithNegatives :: (Num a, Ord a, Traversable t) => t a -> Maybe (t a)
rejectWithNegatives = sequenceA . fmap deleteIfNegative

-- Traversable laws

newtype Identity a = Identity { runIdentity :: a } deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure x = Identity x
  Identity f <*> Identity x = Identity (f x)

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure . pure $ x)
  Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

spec :: Spec
spec = do
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

  describe "WikiBook examples" $ do
    let testList = [-5,3,2,-1,0]
    it "can reject negatives" $ do
      fmap deleteIfNegative testList
        `shouldBe` [Nothing, Just 3, Just 2, Nothing, Just 0]
    it "can reject with Traversable" $ do
      rejectWithNegatives testList `shouldBe` Nothing
      rejectWithNegatives [1..5]
        `shouldBe` Just [1..5]
      rejectWithNegatives [1,-2,3]
        `shouldBe` Nothing
