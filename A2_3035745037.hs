import Text.XHtml (p)
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
