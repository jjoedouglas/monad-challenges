import MCPrelude
import Data.List

type Gen a = Seed -> (a, Seed)


fiveRands :: [Integer]
fiveRands = take 5 $ unfoldr (\b -> Just (rand b)) (mkSeed 1)

randLetter :: Gen Char
randLetter seed = promoteToLetter $ rand seed
  where promoteToLetter (i, s) = (toLetter i, s)

randLetter3 :: String
randLetter3 = take 3 $ unfoldr(\seed -> Just (randLetter seed)) (mkSeed 1)

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
