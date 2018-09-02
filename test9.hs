import Data.Maybe
import Data.List

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I
--------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame xs
  = and [y | y <- (zipWith (\a b -> a == b) xs (tail xs))]

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove x
  = filter (\(a,b) -> x /= a)

-- This code makes sure that correct attribute is returned even if
-- some Attributes share common AttValues.
lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt att h r
  = concat (intersect r (lookUp att h))
-- This also work when rows contain douplicate AttNames.
removeAtt :: AttName -> Header -> Row -> Row
removeAtt att h r
  = r \\ (lookUp att h)

addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping pair@(x, y) ((x', ys) : xs)
  | x == x'   = ((x', (y : ys)) : xs)
  | otherwise = (x', ys) : addToMapping pair xs
addToMapping (x, y) list
  = (x, [y]) : list

buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable (attName, attVal) (header, table)
  = map (\(x,y) -> (x, length y)) frequencies
    where
      attVals = [(lookUpAtt attName header row)| row <- table]
      countList (x : xs) res
        = countList xs (addToMapping (x, 'x') res)
      countList [] res
        = res
      frequencies = countList attVals initialList
      initialList = [(x, []) | x <- attVal]
--------------------------------------------------------------------
-- PART II
--------------------------------------------------------------------

nodes :: DecisionTree -> Int
nodes Null
  = 0
nodes (Leaf _)
  = 1
nodes (Node _ [])
  = 1
nodes (Node name ((_, tree) : xs))
  = (nodes tree) + (nodes (Node name xs))


evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _
  = ""
evalTree (Leaf val) _ _
  = val
evalTree (Node attName xs) h r
  = evalTree tree h r
    where
      attValue = lookUpAtt attName h r
      tree     = lookUp attValue xs
--------------------------------------------------------------------
-- PART III
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

partitionData :: DataSet -> Attribute -> Partition
partitionData (h, t) a@(attName, attVals)
  = [(val, (filter (\x -> x /= a) h, filterTable val)) | val <- attVals] 
    where
      filterTable val = [removeAtt attName h row | row <- t, (elem val row)] 

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree s@(h, t) a@(attName, xs) selector
  | allEmpty t       = Null
  | allSame ress  = (Leaf r)
  | otherwise        = (Node attName' pairs)
    where
      ress@(r : results) = [lookUpAtt "result" h row | row <- t]
      partitions    = partitionData s a'
      pairs         
        = [(val, buildTree s' a' selector) | (val, s') <- partitions]
      a'@(attName', xs') = selector s a

allEmpty :: [Row] -> Bool
allEmpty xs = sum (map (\x -> if null x then 0 else 1) xs) == 0
--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------

entropy :: DataSet -> Attribute -> Double
entropy 
  = undefined

gain :: DataSet -> Attribute -> Attribute -> Double
gain 
  = undefined

bestGainAtt :: AttSelector
bestGainAtt 
  = undefined

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]
