module Hpffp.Madness where

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madLibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madLibbin' e adv noun adj =
    e <> "! he said " <>
    adv <> " as he jumped into his car " <>
    noun <> " and drove off with his " <>
    adj <> " wife."

madLibbinBetter' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madLibbinBetter' e adv noun adj =
    mconcat [e
            , "! he said "
            , adv
            , " as he jumped into his car "
            , noun
            , "and drove off with his "
            , adj
            , " wife."]
