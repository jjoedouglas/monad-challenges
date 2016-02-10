import Prelude (product, sum)
import MCPrelude


data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ show a

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay a [] = Nothing
lookupMay a (ab:abs) = if a == (fst ab) then (Just $ snd ab) else (lookupMay a abs)

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay n d = Just (n / d)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just (maximum x xs)
  where maximum x [] = x
        maximum x (y:ys) = if y > x then maximum y ys else maximum x ys

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = Just (minimum x xs)
  where minimum x [] = x
        minimum x (y:ys) = if y < x then minimum y ys else minimum x ys

queryGreek :: GreekData -> String -> Maybe Double
queryGreek greekData string = case lookupMay string greekData of
  Nothing -> Nothing
  (Just is) -> case tailMay is of
    Nothing -> Nothing
    (Just tailIs) -> case maximumMay tailIs of
      Nothing -> Nothing
      (Just maxTailIs) -> case headMay is of
        Nothing -> Nothing
        (Just headIs) -> case divMay (fromIntegral maxTailIs) (fromIntegral headIs) of
          Nothing -> Nothing
          (Just omg) -> Just omg

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain aToMaybeB (Just a) = aToMaybeB a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing _ = Nothing
link (Just a) aToMaybeB = aToMaybeB a

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 greekData string = div max head
  where xs = lookupMay string greekData
        tail = link xs tailMay
        head = link xs headMay
        max = link tail maximumMay
        div a b = link a (\n -> link b (\d -> divMay (fromIntegral n) (fromIntegral d)))

salaries :: [(String, Integer)]
salaries = [ ("alice", 105000)
           , ("bob", 90000)
           , ("carol", 85000)
           ]

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries [] _ _ = Nothing
addSalaries table name1 name2 = sum
  where firstSalary = lookupMay name1 table
        secondSalary = lookupMay name2 table
        sum = link firstSalary (\s1 -> link secondSalary (\s2 -> Just (s1 + s2)))

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f mA mB =  link mA (\a -> link mB (\b -> (Just $ f a b)))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 table name1 name2 = yLink (+) firstSalary secondSalary
  where firstSalary = lookupMay name1 table
        secondSalary = lookupMay name2 table

mkMaybe :: a -> Maybe a
mkMaybe = Just

tailProd :: Num a => [a] -> Maybe a
tailProd as = transMaybe (product) $ tailMay as

tailSum :: Num a => [a] -> Maybe a
tailSum as = transMaybe sum $ tailMay as 

tailF :: ([a] -> b) -> [a] -> Maybe b
tailF f as = link (tailMay as) (Just . f)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f mA = link mA (Just . f)

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax as = transMaybe maximumMay $ tailMay as

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin as = transMaybe minimumMay $ tailMay as

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just (mA)) = mA
