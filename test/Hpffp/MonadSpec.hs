module Hpffp.MonadSpec where

import Test.Hspec
import Control.Monad (join, liftM2, liftM3)
import Control.Applicative (liftA2, (*>))

main :: IO ()
main = hspec spec

-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
    putStrLn "blah" *>
    putStrLn "another thing"

binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

binding' :: IO ()
binding' =
    getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStrLn "name pls:" >>
    getLine >>=
    \name ->
        putStrLn ("y helo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn "age pls:"
    age <- getLine
    putStrLn ("y helo thar: "
              ++ name ++ " who is: "
              ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
    putStrLn "name pls:" >>
    getLine >>=
    \name ->
    putStrLn "age pls: " >>
    getLine >>=
    \age ->
    putStrLn ("y helo thar: "
              ++ name ++ " who is: "
              ++ age ++ " years old.")

spec :: Spec
spec = do
    describe "Monads" $ do
        it "fmap and applicatives can be combined to Monads" $ do
            let a = fmap (+1) [1..3]
                b = [1..3] >>= return . (+1)
            a `shouldBe` b
        it "can be very similar to Functors" $ do
            {-
                fmap :: Functor f => (a -> b) -> f a -> f b
                <*>  :: Applicative f => f (a -> b) -> f a -> f b
                >>=  :: Monad f => f a -> (a -> f b) -> f b
            -}
            let andOne x = [x, 1]
            andOne 10 `shouldBe` [10, 1]
            fmap andOne [4..6] `shouldBe` [[4,1],[5,1],[6,1]]
            -- when we want to deiscard one layer of that structure
            concat (fmap andOne [4..6]) `shouldBe` [4,1,5,1,6,1]
        it "can use join as a generalization of concat" $ do
            {-
                concat :: Foldable t => t [a] -> a
                - or -
                concat :: [[a]] -> [a]

                join :: Monad m => m (m a) -> m a
            -}
            let andOne x = [x,1]
            bind andOne [1,2,3] `shouldBe` [1,1,2,1,3,1]
        it "is also uses 'lift' functions" $ do
            {-
                liftA :: Applicative f => (a -> b) -> f a -> f b
                liftM :: Monad m => (a1 -> r) -> m a1 -> m r
                liftA2 :: Applicative f => (a -> b -> c) -> fa -> fb -> fc
                liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
                liftA3 :: Applicative f => (a -> b -> c -> d)
                                            -> f a -> f b
                                            -> f c -> f d
                liftM3 :: Monad m => (a1 -> a2 -> a3 -> r)
                                     -> m a1 -> m a2
                                     -> m a3 -> m r
            -}
            liftA2 (,) (Just 3) (Just 5) `shouldBe` Just (3,5)
            liftM2 (,) (Just 3) (Just 5) `shouldBe` Just (3,5)
            zipWith (+) [3,4] [5,6] `shouldBe` [8,10]
            liftA2 (+) [3,4] [5,6] `shouldBe` [8,9,9,10]
            liftM3 (,,) [1,2] [3] [5,6]
                `shouldBe` [(1,3,5),(1,3,6),(2,3,5),(2,3,6)]
            zipWith3 (,,) [1,2] [3] [5,6]
                `shouldBe` [(1,3,5)]
        it "can use do syntax, but does not have to" $ do
            {-
                (*>) :: Applicative f => f a -> f b -> f b
                (>>) :: Monad m => m a -> m b -> m b
            -}
            pending

