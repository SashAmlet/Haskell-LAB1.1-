import qualified Data.Map as Map

-- A function to count the number of occurrences of each element of list B
-- Receives as input a list of numbers, and as output returns pairs of the type ('number', 'number of occurrences of a number')
countOccurrences :: [Int] -> Map.Map Int Int 
countOccurrences = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty

-- Function for filtering list A
filterList :: [Int] -> [Int] -> [Int]
filterList listA listB = filter (`Map.member` singleOccurrenceMap) listA
  where singleOccurrenceMap = Map.filter (==1) $ countOccurrences listB

-- Example of use
main :: IO ()
main = do
  let listA = [1, 2, 3, 4, 5, 6, 7]
  let listB = [1, 2, 2, 3, 3, 4, 4, 4, 5]
  print $ filterList listA listB