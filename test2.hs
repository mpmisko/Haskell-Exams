import Data.List hiding (insert)

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix s s'
   = elem s (inits s')
    
removePrefix :: String -> String -> String
removePrefix prefix s
  = removePrefix' [] s
    where 
      removePrefix' p s'@(x : xs)
        | p == prefix = s'
        | otherwise   = removePrefix' (p ++ [x]) xs
      removePrefix' p [] = []

suffixes :: [a] -> [[a]]
suffixes xs
  = filter (not . null) (tails xs)

isSubstring :: String -> String -> Bool
isSubstring s
  = (or . isPrefixOfAnySuffix s)

isPrefixOfAnySuffix s s' = [isPrefix s x | x <- (suffixes s')]

findSubstrings :: String -> String -> [Int]
findSubstrings s s'
  = [x | x <- [0..(length s') - 1], ((isPrefixOfAnySuffix s s') !! x)]
------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf n) 
  = [n]
getIndices (Node ((s, tree) : xs)) 
  = (getIndices tree) ++ (getIndices (Node xs))
getIndices (Node []) 
  = []

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition 
  = undefined

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' s (Leaf i)
  | null s    = [i]
  | otherwise = []
findSubstrings' s sTree@(Node (list))
  = concatMap f list
    where
      f (nodeS, tree)
        | isPrefix s nodeS 
          = getIndices tree
        | isPrefix nodeS s 
          = findSubstrings' (removePrefix nodeS s) tree
        | otherwise
          = []
------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert node@(s, index) (Node (first@(s', tree) : xs))
  | null commonPrefix
    = (append first (insert node (Node xs)))
  | commonPrefix == s' 
    = Node ((s', (insert (removePrefix s' s, index) tree)) : xs)
  | otherwise
    = Node ((commonPrefix, (Node [(dropPreS, (Leaf index)), (dropPreS', tree)])) : xs) 
  where
    commonPrefix   = maximum (intersect (inits s) (inits s'))
    dropPreS       = removePrefix commonPrefix s
    dropPreS'      = removePrefix commonPrefix s'
    append n (Node ns) = Node (n : ns)
insert (s, index) (Node ns)
   = Node ((s, (Leaf index)) : ns)

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]


