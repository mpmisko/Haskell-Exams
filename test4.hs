import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp x xs
  = fromJust (lookup x xs)

states :: LTS -> [State]
states
  = nub . (concatMap (\((s, s'), _) -> [s,s']))

transitions :: State -> LTS -> [Transition]
transitions state
  = filter (\((s,_),_) -> s == state ) 

alphabet :: LTS -> Alphabet
alphabet 
  = nub . map(\((_,_), id) -> id)

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions (STOP)
  = []
actions (Ref _)
  = []
actions (Prefix id process)
  = id : (actions process)
actions (Choice processes)
  = concatMap (\x -> (actions x)) processes

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts ids defs@((_, process) : xs)
  = accepts' ids process
  where
    accepts' :: [Id] -> Process -> Bool
    accepts' [] _                  
      = True
    accepts' i (Choice pros)       
      = or (map (\x -> (accepts' i x)) pros)
    accepts' i (Ref x)    
      = accepts' i (lookUp x defs)
    accepts' (id : ids) (Prefix x process)
      = (id == x) && (accepts' ids process)
    accepts' _ (STOP) = False
------------------------------------------------------
-- PART III

--composeTransitions :: Transition -> Transition 
--                   -> Alphabet -> Alphabet 
--                   -> StateMap 
--                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s,t), a) ((s',t'), a') alfa alfa' smap
  | a == a'                          
    = [((map (s, s'), (map (t, t'))),a)]
  | (elem a alfa') && (elem a' alfa) 
    = []
  | (elem a' alfa)                   
    = [((map (s, s'), (map (t, s'))),a)]
  | (elem a alfa')                   
    = [((map (s, s'), (map (s, t'))),a')]
  | otherwise
    = [((map (s, s'), (map (t, s'))),a), ((map (s, s'), (map (s, t'))),a')]
  where
    map (x, y) = lookUp (x,y) smap
pruneTransitions :: [Transition] -> LTS
pruneTransitions ts
  = visit 0 []
  where
    visit :: State -> [State] -> [Transition]
    visit s ss
      | not (s `elem` ss) 
        = solution ++ (concatMap (\((f, t), id) -> visit t (f : ss)) solution) 
      | otherwise       
        = []
        where
          solution = transitions s ts
------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose l l' = (nub . pruneTransitions) result
  where
    product        
      = cartesian (states l) (states l')
    transPair      
      = [((transitions x l), (transitions y l')) | x <- (states l), y <- (states l')]
    transCart 
      = [(cartesian x y) | (x,y) <- transPair]
    result       
      = concatMap (\(t,t') -> compose t t') (concat transCart)
    compose t t'
      | (null t) && (null t') 
        = composeTransitions s s' ("$" : (alphabet l)) ("$'" : (alphabet l')) mapS
      | otherwise = composeTransitions t t' (alphabet l) (alphabet l') mapS 
    mapS         
      = map (\x -> (x, fromJust (elemIndex x product))) product
    cartesian xs ys 
      = [(x,y) | x <- xs, y <- ys]
    s  = ((0,0),"$")
    s' = ((0,0),"$'")
------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS 
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]

