{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)


fiveRands :: [Integer]
fiveRands = [(fst r1), (fst r2), (fst r3), (fst r4), (fst r5)]
  where r1 = rand (mkSeed 1)
        r2 = rand (snd r1)
        r3 = rand (snd r2)
        r4 = rand (snd r3)
        r5 = rand (snd r4)

randLetter :: Gen Char
randLetter seed = promoteToLetter $ rand seed
  where promoteToLetter (i, s) = (toLetter i, s)

randLetter3 :: String
randLetter3 = map toLetter $ take 3 $ fiveRands

generalA :: Gen a -> (a -> b) -> Gen b
generalA g f seed = promote $ g seed
  where promote (i, s) = (f i, s)

randEven :: Gen Integer
randEven = generalA rand (* 2)

randOdd :: Gen Integer
randOdd = generalA rand (\x -> x * 2 + 1)

randTen :: Gen Integer
randTen = generalA rand (* 10)

randPair :: Gen (Char, Integer)
randPair seed = buildTuple randChar randInteger
  where randInteger = rand $ snd randChar
        randChar = randLetter seed
        buildTuple (c, _) (i, s) = ((c, i), s)

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair = generalB (,) 

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f gA gB seed = apply randA randB
  where randA = gA seed
        randB = gB $ snd randA
        apply (a, _) (b, s) = ((f a b), s)

repRandom :: [Gen a] -> Gen [a]
repRandom [] seed = ([], seed)
repRandom (gA:gAs) seed = ( fst randA : fst rest, snd rest)
  where randA = gA seed
        rest = repRandom gAs $ snd randA

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo gA f seed = f (fst randA) $ snd randA
  where randA = gA seed


mkGen :: a -> Gen a
mkGen a = \seed -> (a, seed)
