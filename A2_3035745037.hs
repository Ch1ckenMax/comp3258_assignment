-- Question 0 Propositional Logic

type Name = String
type Env  = [(Name, Bool)]
data Prop = Var Name
            | F
            | T
            | Not Prop
            | Prop :|: Prop
            | Prop :&: Prop
            | Prop :->: Prop
            | Prop :<->: Prop
                deriving (Eq, Show)

-- Problem 1
eval :: Env -> Prop -> Bool
eval env (Var varname) = case lookup varname env of
                           Just b -> b
                           Nothing -> error ("Variable " ++ show varname ++ " not found in environment")
eval env F             = False
eval env T             = True
eval env (Not p)       = not (eval env p)
eval env (p1 :|: p2)   = eval env p1 || eval env p2
eval env (p1 :&: p2)   = eval env p1 && eval env p2
eval env (p1 :->: p2)  = not (eval env p1) || eval env p2
eval env (p1 :<->: p2) = eval env p1 == eval env p2

testEnv :: Env
testEnv = [("p", True), ("q", False)]

-- Problem 2

-- Given two lists of names with unique elements, append two lists with duplicate elements removed
getUniqueNames :: [Name] -> [Name] -> [Name]
getUniqueNames xs []         = xs
getUniqueNames [] ys         = ys
getUniqueNames (x:xs) ys = if x `elem` ys then getUniqueNames xs ys else x : getUniqueNames xs ys

-- Given a propositions, find all the variables used
getNameList :: Prop -> [Name]
getNameList (Var varname) = [varname]
getNameList T             = []
getNameList F             = []
getNameList (Not p)       = getNameList p
getNameList (p1 :|: p2)   = getUniqueNames (getNameList p1) (getNameList p2)
getNameList (p1 :&: p2)   = getUniqueNames (getNameList p1) (getNameList p2)
getNameList (p1 :->: p2)  = getUniqueNames (getNameList p1) (getNameList p2)
getNameList (p1 :<->: p2) = getUniqueNames (getNameList p1) (getNameList p2)

-- Given a list of names, generate all possible environments
generateAllPossibleEnvironments :: [Name] -> [Env]
generateAllPossibleEnvironments []     = []
generateAllPossibleEnvironments [x]    = [[(x, True)], [(x, False)]]
generateAllPossibleEnvironments (x:xs) = map ((x, True):) envList ++ map ((x, False):) envList
                                           where
                                             envList = generateAllPossibleEnvironments xs

-- Given a propositon, evaluates the result w.r.t each possible possible environments
evalAllPossibleEnv :: Prop -> [Bool]
evalAllPossibleEnv p = [eval env p | env <- generateAllPossibleEnvironments $ getNameList p]

satisfiable :: Prop -> Bool
satisfiable p = or $ evalAllPossibleEnv p

unsatisfiable :: Prop -> Bool
unsatisfiable p = all not $ evalAllPossibleEnv p

valid :: Prop -> Bool
valid p = and $ evalAllPossibleEnv p

testProp1 = (Var "p") :&: (Var "q")
testProp2 = (Var "p") :|: (Var "q")
testProp3 = (Var "p") :&: (Var "p")
testProp4 = ((Var "q") :|: (Var "p")) :&: (Var "p")

-- Problem 3
negateProp :: Prop -> Prop
negateProp (Var varname) = Not (Var varname)
negateProp T             = F
negateProp F             = T
negateProp (Not p)       = p
negateProp (p1 :|: p2)   = negateProp p1 :&: negateProp p2
negateProp (p1 :&: p2)   = negateProp p1 :|: negateProp p2
negateProp (p1 :->: p2)  = p1 :&: negateProp p2
negateProp (p1 :<->: p2) = (p1 :&: negateProp p2) :|: (negateProp p1 :&: p2)

toDNF :: Prop -> [[Prop]]
toDNF (Var n)      = [[Var n]]
toDNF T            = [[T]]
toDNF F            = [[F]]
toDNF (Not p)      = case p of
                       Var n -> [[Not (Var n)]]
                       T     -> [[F]]
                       F     -> [[T]]
                       p'     -> toDNF $ negateProp p' 
toDNF (p1 :|: p2)  = toDNF p1 ++ toDNF p2
toDNF (p1 :&: p2)  = [p1Clause ++ p2Clause | p1Clause <- toDNF p1, p2Clause <- toDNF p2]
toDNF (p1 :->: p2) = toDNF (Not p1) ++ toDNF p2
toDNF (p1 :<->: p2) = toDNF (p1 :&: p2) ++ toDNF (Not p1 :&: Not p2)


-- Question 1 Heap Sort

data Tree a = Leaf
            | Branch a (Tree a) (Tree a)
     deriving (Show, Eq)

flatten :: Ord a => Tree a -> [a]
flatten Leaf           = []
flatten (Branch x l r) = x : merge (flatten l) (flatten r)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys                     = ys
merge xs []                     = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

buildHeap :: Ord a => [a] -> Tree a
buildHeap = heapify . buildTree

buildTree :: Ord a => [a] -> Tree a
buildTree []     = Leaf
buildTree (x:xs) = Branch x (buildTree leftList) (buildTree rightList)
               where
                 (leftList, rightList) = splitAt (length xs `div` 2) xs 

heapify :: Ord a => Tree a -> Tree a
heapify Leaf           = Leaf
heapify (Branch x l r) = siftDown x (heapify l) (heapify r)

siftDown :: Ord a => a -> Tree a -> Tree a -> Tree a
siftDown x Leaf Leaf                                              = Branch x Leaf Leaf
siftDown x Leaf (Branch x' t1 t2) | x > x'                        = Branch x' Leaf (siftDown x t1 t2)
                                  | otherwise                     = Branch x Leaf (Branch x' t1 t2)
siftDown x (Branch x' t1 t2) Leaf | x > x'                        = Branch x' (siftDown x t1 t2) Leaf
                                  | otherwise                     = Branch x (Branch x' t1 t2) Leaf
siftDown x (Branch x1 t1 t2) (Branch x2 t3 t4) | x < x1 && x < x2 = Branch x (Branch x1 t1 t2) (Branch x2 t3 t4) 
                                               | x1 < x2          = Branch x1 (siftDown x t1 t2) (Branch x2 t3 t4)
                                               | x1 >= x2         = Branch x2 (Branch x1 t1 t2) (siftDown x t3 t4)
                                  
heapSort :: Ord a => [a] -> [a]
heapSort = flatten . buildHeap


-- Question 2 Virtual Machine

data Value = Numeric Int
            | Wrong
        deriving (Eq, Show, Read)

type Trace a = [Event a]
data Event a = Output a
             | End
             | Crash
             | Tick
        deriving (Eq, Show)

data Instruction = Push Value
                 | Pop
                 | Fetch Int
                 | Store Int
                 | Display
                 | Halt
                 | Jump Int
            deriving (Eq, Show, Read)

exec :: [Instruction] -> Trace Value
exec instrs = snd (run instrs 0 [])

execDebug :: [Instruction] -> ([Value], Trace Value)
execDebug instrs = run instrs 0 []

run :: [Instruction] -> Int -> [Value] -> ([Value], Trace Value)
run pg pc st
    | pc < 0 || length pg <= pc = (st, [Crash])
    | pg !! pc == Halt          = (st, [End])
    | otherwise                 = let (pc', st', tr') = step pg pc st
                                      (st'', tr'') = run pg pc' st'
                                  in (st'', tr' +++ tr'')

-- Problem 4
-- Given two traces, if we just concat two traces together like concatening lists, the End or Crash event of the first trace will appear in the middle of the concatened list, so the invariant is broken.

(+++) :: Trace a -> Trace a -> Trace a
s +++ t = case last s of
    End      -> init s ++ t -- Since trace s has been terminated normally, we run on into the trace t
    Crash    -> s           -- Since trace s has been terminated abnormally, we throw away the trace t
    _        -> error "The first trace does not end with End or Crash"

-- Problem 5 to Problem 8
replace :: Int -> a -> [a] -> [a]
replace n x xs | n < 0 || length xs <= n = error "Invalid replace index"
               | otherwise               = take n xs ++ [x] ++ drop (n + 1) xs

step :: [Instruction] -> Int -> [Value] -> (Int, [Value], Trace Value)
step pg pc st =
    case (pg !! pc, st) of
        (Push x , stack)      -> (pc', x : stack, [End])
        (Pop , _ : stack)     -> (pc', stack, [End])
        (Fetch n , stack)
          | length stack > n  -> (pc', stack !! n : stack, [End])
        (Store n , x : stack)
          | length stack > n  -> (pc', replace n x stack, [End])
        (Display , i : stack) -> (pc', stack, [Output i, End])
        (Jump k, stack)       -> if k >= 0 then (pc' + k, stack, [End]) else (pc' + k, stack, [Tick, End])
        (_ , stack)           -> (pc', stack, [Crash])
      where
        pc' = pc + 1


-- Question 3 Battleship Game


