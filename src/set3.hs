{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a, b)]
allPairs = allCombs (,)

data Card = Card Int String

instance Show Card where
  show (Card i s) = (show i) ++ s

allCards :: [Int] -> [String] -> [Card]
allCards = allCombs (Card)

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f as bs = combStep (map f as) bs

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs cs = combStep (combStep (map f as) bs) cs

combStep :: [a -> b] -> [a] -> [b]
combStep fs as = foldr (++) [] (map (\f -> map (\a -> f a) as) fs)

allCombs4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4 f as bs cs ds = combStep (combStep (combStep (map f as) bs) cs) ds
