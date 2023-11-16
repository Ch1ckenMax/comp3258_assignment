import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Binop = Add | Sub | Mul | Div | Mod deriving (Eq, Show)
data Expr = Bin Binop Expr Expr
          | Val Int
          | Var String deriving (Eq, Show)

type Env = [(String, Int)]

type Data = (String, -- type name
             [String], -- parameters
             [(String, [Type])]) -- constructors and arguments

data Type = Arrow Type Type -- function
          | Apply Type Type -- application
          | VarT String -- variable
          | Con String -- constructor
          | Tuple [Type] -- tuple
          | List Type deriving Show -- list

-- Monad and functions for parser. Copied from the lecture code
data Parser a =  P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) = p

failure :: Parser a
failure = P (\inp -> []) 

(+++) :: Parser a -> Parser a -> Parser a
p +++ q =  P (\inp ->
                case parse p inp of
                  []  -> parse q inp
                  r   -> r
             )

item :: Parser Char
item =  P (\inp ->
             case inp of
               []     -> []
               (x:xs) -> [(x,xs)]
          )

token  :: Parser a -> Parser a
token p =  do space
              v <- p
              space
              return v

space :: Parser ()
space =  do many (sat isSpace)
            return ()

nat :: Parser Int
nat =  do xs <- many1 digit
          return (read xs)

int :: Parser Int
int =  do char '-'
          n <- nat
          return (-n)
        +++ nat

string :: String -> Parser String
string []     = return ""
string (x:xs) = do char x
                   string xs
                   return (x:xs)  

digit :: Parser Char
digit = sat isDigit

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c then return c else failure 

char :: Char -> Parser Char
char c = sat (\x -> x == c) 

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do x  <- p
             xs <- many p
             return (x:xs)

instance Functor Parser where
   fmap f (P g) = P (\s -> fmap (\(x,s') -> (f x, s')) (g s))

instance Applicative Parser where
   pure   = return
   (<*>)  = ap

instance Alternative Parser where
   empty = failure
   (<|>) = (+++)

instance Monad Parser where
   return v =  P (\inp -> [(v,inp)])
   p >>= f  =  P (\inp -> case parse p inp of
                             []        -> []
                             [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where
   mzero        =  P (\inp -> [])
   p `mplus` q  =  P (\inp -> case parse p inp of
                                 []        -> parse q inp
                                 [(v,out)] -> [(v,out)]) 

-- Problem 1
eval :: Env -> Expr -> Maybe Int
eval e (Bin op e1 e2) = do left  <- eval e e1
                           right <- eval e e2
                           case op of
                             Add -> return $ left + right
                             Sub -> return $ left - right
                             Mul -> return $ left * right
                             Div -> if right == 0 then Nothing else return $ left `div` right
                             Mod -> if right == 0 then Nothing else return $ left `mod` right
eval e (Val x)        = return x
eval e (Var varname)  = lookup varname e

-- Problem 2

-- I modified the grammar a little bit
-- expr    := term op_term*
-- op_term := ('*' | '/' | '%') term
-- where op_term* means that op_term appears 0 or more times
-- Likewise:
-- term      := factor op_factor*
-- op_factor := ('*' | '/' | '%') factor

pExpr :: Parser Expr
pExpr = do left    <- term
           opTerms <- many opTerm
           return $ foldl combineExpr left opTerms

opTerm :: Parser (Binop, Expr)
opTerm = do op <- (token . char $ '+') +++ (token . char $ '-')
            e  <- term
            case op of
              '+' -> return (Add, e)
              '-' -> return (Sub, e)

term :: Parser Expr
term = do left      <- factor
          opFactors <- many opFactor
          return $ foldl combineExpr left opFactors

opFactor :: Parser (Binop, Expr)
opFactor = do op <- (token . char $ '*') +++ (token . char $ '/') +++ (token . char $ '%')
              e  <- factor
              case op of
                '*' -> return (Mul, e)
                '/' -> return (Div, e)
                '%' -> return (Mod, e)

factor :: Parser Expr
factor = (do n <- token int
             return (Val n)) +++ (do token $ char '('
                                     e <- pExpr
                                     token $ char ')'
                                     return e) +++ token (do c  <- sat isLower
                                                             cs <- many $ sat isAlphaNum
                                                             return (Var $ c:cs))

combineExpr :: Expr -> (Binop, Expr) -> Expr
combineExpr e1 (op, e2) = Bin op e1 e2


-- Problem 3

optimize :: Expr -> Maybe Expr
optimize (Bin Add e1 e2) = do o1 <- optimize e1 
                              o2 <- optimize e2 
                              case (o1, o2) of
                                (Val 0, _)     -> return o2 
                                (_, Val 0)     -> return o1 
                                (Val a, Val b) -> return $ Val $ a + b
                                _              -> return $ Bin Add o1 o2
optimize (Bin Sub e1 e2) = do o1 <- optimize e1 
                              o2 <- optimize e2 
                              case (o1, o2) of
                                (_, Val 0)     -> return o1 
                                (Val a, Val b) -> return $ Val $ a - b
                                _              -> return $ Bin Sub o1 o2
optimize (Bin Mul e1 e2) = do o1 <- optimize e1 
                              o2 <- optimize e2 
                              case (o1, o2) of
                                (Val 0, _)     -> return $ Val 0
                                (_, Val 0)     -> return $ Val 0
                                (Val a, Val b) -> return $ Val $ a * b
                                _              -> return $ Bin Mul o1 o2
optimize (Bin Div e1 e2) = do o1 <- optimize e1 
                              o2 <- optimize e2 
                              case (o1, o2) of
                                (_, Val 0)     -> Nothing
                                (Val a, Val b) -> return $ Val $ a `div` b
                                _              -> return $ Bin Div o1 o2
optimize (Bin Mod e1 e2) = do o1 <- optimize e1 
                              o2 <- optimize e2 
                              case (o1, o2) of
                                (_, Val 0)     -> Nothing
                                (Val a, Val b) -> return $ Val $ a `mod` b
                                _              -> return $ Bin Mod o1 o2
optimize expr            = return expr


-- Problem 4

pDatatype :: Parser Data
pDatatype = do token $ string "data"
               typeName         <- constructor
               variables        <- many variable
               token $ char '='
               dataConstructors <- pDataConstructors
               return (typeName, variables, dataConstructors)

pDataConstructor :: Parser (String, [Type])
pDataConstructor = do con  <- constructor
                      args <- many pElement
                      return (con, args)

pDataConstructors :: Parser [(String, [Type])]
pDataConstructors = do pd <- pDataConstructor
                       (do token $ char '|'
                           pds <- pDataConstructors
                           return $ pd:pds) +++ return [pd]

isValidCharForVar :: Char -> Bool 
isValidCharForVar c = elem c "_\'" || isAlphaNum c
               
constructor :: Parser String
constructor = token (do c  <- sat isUpper
                        cs <- many $ sat isValidCharForVar
                        return (c:cs))

variable :: Parser String
variable = token (do c  <- sat isLower
                     cs <- many $ sat isValidCharForVar
                     return (c:cs))

-- Grammar:
-- pType := pElement ('' | "->" pType | pElement* | pElement* "->" pType)
-- pElement := pVarT | pCon | '(' pType {',' pType} ')' | '[' pType ']'

pType :: Parser Type
pType = token $ do t1 <- pElement
                   -- pType := pElement "->" pType
                   (do token $ string "->"
                       t2 <- pType
                       return $ Arrow t1 t2)
                           -- pType := pElement pElement* "->" pType 
                       +++ (do pes <- many1 pElement
                               token $ string "->"
                               t3 <- pType
                               return $ Arrow (foldl Apply t1 pes) t3)    
                                   -- pType := pElement pElement*                         
                               +++ (do ts <- many1 pElement
                                       return $ foldl Apply t1 ts) 
                                       -- pType := pElement
                                       +++ return t1

pElement :: Parser Type
pElement = pVarT +++ pCon +++ pBracket +++ pList

pVarT :: Parser Type 
pVarT = do name <- variable
           return $ VarT name

pCon :: Parser Type
pCon = do name <- constructor
          return $ Con name

tuple :: Parser [Type]
tuple = do t <- pType
           (do token $ char ','
               ts <- tuple
               return (t:ts)) +++ return [t]

pBracket :: Parser Type
pBracket = do token $ char '('
              ts <- tuple 
              token $ char ')'
              if length ts == 1 then return $ head ts else return $ Tuple ts
            
pList :: Parser Type
pList = do token $ char '['
           t <- pType 
           token $ char ']'
           return $ List t