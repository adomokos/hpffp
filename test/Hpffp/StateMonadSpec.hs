module Hpffp.StateMonadSpec where

import Test.Hspec
import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

main :: IO ()
main = hspec spec

{- newtype State s a = -}
    {- State { runState :: s -> (a, s) } -}

{-
   Similar to Reader:
newtype Reader r a =
    Reader { runReader :: r -> a }

State :: (s -> (a, s)) -> State s a

runState :: State s a -> s -> (a, s)

-- Random looks a lot like State
random :: (Random a) => StdGen -> (a, StdGen)
State { runState :: s -> (a, s) }

randomR :: (...) => (a, a) -> g -> (a, g)
State {    runState ::        s -> (a, s) }
-}

-- Six-sided die
data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
    case n of
      1 -> DieOne
      2 -> DieTwo
      3 -> DieThree
      4 -> DieFour
      5 -> DieFive
      6 -> DieSix
      x -> error $
            "intToDie got non 1-6 integer: "
            ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
    let s = mkStdGen 0
        (d1, s1) = randomR (1, 6) s
        (d2, s2) = randomR (1, 6) s1
        (d3, _)  = randomR (1, 6) s2
    (intToDie d1, intToDie d2, intToDie d3)

{-
   The `state` function is a constructor that
   takes a State-like function and embeds it
   in the State monad transformer.
   state :: Monad m
         => (s -> (a, s))
         -> StateT s m a
-}

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' =
    intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
    liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
            | sum >= limit = count
            | otherwise =
                let (die, nextGen) =
                      randomR (1, 6) gen
                   in go (sum + die) (count + 1) nextGen

rollsToGetLogged :: Int -> StdGen -> (Int, [Die])
rollsToGetLogged limit g = go 0 (0, []) g
    where
        go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go sum (count, rolls) gen
            | sum >= limit = (count, rolls)
            | otherwise =
                let (die, nextGen) =
                      randomR (1, 6) gen
                   in go (sum + die)
                         ((count + 1), (intToDie die) : rolls)
                         nextGen

spec :: Spec
spec = do
    describe "State monad" $ do
        it "works with Random" $ do
            pending
