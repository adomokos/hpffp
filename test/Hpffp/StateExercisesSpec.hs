{-# LANGUAGE InstanceSigs #-}

module Hpffp.StateExercisesSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

newtype State s a = State { runState :: s -> (a, s) }

get :: State s s
get = State $ \x -> (x, x)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

exec :: State s a -> s -> s
exec (State sa) x = let (_, s) = sa x
                    in s

eval :: State s a -> s -> a
eval (State sa) x =
    let (y, _) = sa x
     in y

modify :: (s -> s) -> State s ()
modify f = State $ \x -> ((), f x)

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State g) = State $ \s -> let (a, b) = g s
                                     in (f a, b)

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a, s)
    (<*>) :: State s (a -> b)
          -> State s a
          -> State s b
    (State f) <*> (State g) = State $ \s -> let (fab, s') = f s
                                                (a, s'') = g s'
                                            in (fab a, s'')

instance Monad (State s) where
    return = pure
    (>>=) :: State s a
          -> (a -> State s b)
          -> State s b
    (State f) >>= g = State $ \s -> let (a, s') = f s
                                        ms = runState $ g a
                                    in ms s'

    (>>) :: State s a
         -> State s b
         -> State s b
    State f >> State g = State $ \s -> let (_, s') = f s
                                        in g s'

spec :: Spec
spec = do
    describe "State Exercises" $ do
        it "returns the same state" $ do
            runState get "curryIsAmaze"
                `shouldBe` ("curryIsAmaze", "curryIsAmaze")
        it "puts the state" $ do
            runState (put "blah") "woot"
                `shouldBe` ((), "blah")
        it "runs the State with s" $ do
            exec (put "wilma") "daphne" `shouldBe` "wilma"
            exec get "scooby papu" `shouldBe` "scooby papu"
        it "runs the State with s and gets the result" $ do
            eval get "bunnicula" `shouldBe` "bunnicula"
            eval get "stake a bunny" `shouldBe` "stake a bunny"
        it "can create a new State" $ do
            runState (modify (+1)) 0 `shouldBe` ((), 1)
            runState (modify (+1) >> modify (+1)) 0
                `shouldBe` ((),2)
