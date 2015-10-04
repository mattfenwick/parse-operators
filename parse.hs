import Prelude hiding ((<$>), (<*>), (*>), (<*), (>>=), (>>))
import Data.List (intercalate)

newtype Parser t a = Parser {
    unParser :: [t] -> Maybe ([t], a)
}

item :: Parser t t
item = Parser (\xs -> case xs of (y:ys) -> Just (ys, y); _ -> Nothing)

pSym :: Eq t => t -> Parser t t
pSym s = Parser f
  where
    f (x:xs)
      | s == x = Just (xs, x)
    f _ = Nothing

pReturn :: a -> Parser t a
pReturn x = Parser (\xs -> Just (xs, x))

pFail :: Parser t a
pFail = Parser (const Nothing)

infixl 1 >>=
(>>=) :: Parser t a -> (a -> Parser t b) -> Parser t b
Parser p1 >>= f = Parser (\xs -> case (p1 xs) of
                                      Nothing -> Nothing
                                      Just (ys, v) -> unParser (f v) ys)

infixl 1 >>
(>>) :: Parser t a -> Parser t b -> Parser t b
p1 >> p2 = p1 >>= (\_ -> p2)

infixl 5 <*>
(<*>) :: Parser t (a -> b) -> Parser t a -> Parser t b
p1 <*> p2 = 
    p1 >>= \f -> 
    p2 >>= \x ->
    pReturn (f x)

infixr 3 <|>
(<|>) :: Parser t a -> Parser t a -> Parser t a
Parser p1 <|> Parser p2 = Parser (\xs -> let r1 = p1 xs in case r1 of 
                                                                Nothing -> p2 xs
                                                                _ -> r1)

pChoice :: [Parser t a] -> Parser t a
pChoice = foldr (<|>) pFail

pCheck :: (a -> Bool) -> Parser t a -> Parser t a
pCheck pred p = 
    p >>= \x -> if (pred x) 
                then pReturn x
                else pFail

infix 7 <$>
(<$>) :: (a -> b) -> Parser t a -> Parser t b
f <$> Parser p = Parser (\xs -> case (p xs) of 
                                     Just (ys, r) -> Just (ys, f r)
                                     _ -> Nothing)

opt :: Parser t a -> a -> Parser t a
opt p x = p <|> pReturn x

optM :: Parser t a -> Parser t (Maybe a)
optM p = opt (Just <$> p) Nothing

infixl 5 <*
(<*) :: Parser t a -> Parser t b -> Parser t a
p1 <* p2 = const <$> p1 <*> p2

infixl 5 *>
(*>) :: Parser t a -> Parser t b -> Parser t b
p1 *> p2 = flip const <$> p1 <*> p2

infixl 7 <$
(<$) :: (a -> b) -> Parser t z -> Parser t (a -> b)
f <$ p = pReturn f <* p

pSyms :: Eq t => [t] -> Parser t [t]
pSyms [] = pReturn []
pSyms (x:xs) = (:) <$> pSym x <*> pSyms xs

{-
pParens :: Parser Char a -> Parser Char a
pParens p = id <$ pSym '(' <*> p <* pSym ')'

parens = (max . (1 +)) <$> pParens parens <*> opt parens 0
-}

pMany0 :: Parser t a -> Parser t [a]
-- pMany0 p = (:) <$> p <*> opt (pMany0 p) []
pMany0 p = opt ((:) <$> p <*> pMany0 p) []

pMany1 :: Parser t a -> Parser t [a]
pMany1 p = (:) <$> p <*> pMany0 p

pSatisfy :: (t -> Bool) -> Parser t t
pSatisfy pred = Parser (\xs -> case xs of
                                    (y:ys)
                                      | pred y -> Just (ys, y)
                                    _ -> Nothing)

pParens' :: Parser Char Int
pParens' = (\ _ c _ -> c + 1) <$> pSym '(' <*> parens' <*> pSym ')'

parens' :: Parser Char Int
-- parens' = max <$> opt pParens' 0 <*> opt parens' 0
parens' = foldr max 0 <$> pMany0 pParens'

pInteger :: Parser Char Int
pInteger = read <$> pMany1 (pSatisfy (\x -> x <= '9' && x >= '0'))

data T a b
    = Leaf a
    | One b (T a b)
    | OnePost b (T a b)
    | Two b (T a b) (T a b)
    | Three b (T a b) (T a b) (T a b)
  deriving (Show)

showT :: (Show a, Show b) => T a b -> String
showT (Leaf x) = show x
showT (One op arg) = "(" ++ opStr op ++ " " ++ showT arg ++ ")"
showT (OnePost op arg) = "(" ++ showT arg ++ " " ++ opStr op ++ ")"
showT (Two op l r) = "(" ++ showT l ++ " " ++ opStr op ++ " " ++ showT r ++ ")"
showT (Three op l m r) = "(" ++ opStr op ++ " " ++ showT l ++ " " ++ showT m ++ " " ++ showT r ++ ")"

opStr :: Show a => a -> String
opStr = filter ((/=) '"') . show

{-
1 - 2 - 3
(1 - 2) - 3

4 ** 5 ** 6
4 ** (5 ** 6)

4 + 3 ** 2 ** 1 / 8 - 4 * 5
(4 + ((3 ** (2 ** 1)) / 8)) - (4 * 5)

3 ? 4 : 5 ? 6 : 7
3 ? 4 : (5 ? 6 : 7)

3 ? 4 : 5
3 ? 8 : 9 ? 4 : 5
-}

chainR :: Parser t (a -> a -> a) -> Parser t a -> Parser t a
chainR op p = (f <$> p <*> op <*> chainR op p) <|> p -- TODO make more efficient -- factor out the `p`
  where
    f a b c = b a c

chainL :: Parser t (a -> a -> a) -> Parser t a -> Parser t a
chainL op p = flip ($) <$> p <*> rest
  where
    f b c rs a = rs (b a c)
    rest = (f <$> op <*> p <*> rest) <|> pReturn id

sepBy1 :: Parser t a -> Parser t b -> Parser t [b]
sepBy1 sep p = (:) <$> p <*> (pMany0 (sep *> p))

sepBy0 :: Parser t a -> Parser t b -> Parser t [b]
sepBy0 sep p = sepBy1 sep p <|> pReturn []

prefix :: Parser t (a -> a) -> Parser t a -> Parser t a
prefix op p = (op <*> prefix op p) <|> p

postfix :: Parser t (a -> a) -> Parser t a -> Parser t a
postfix op p = flip ($) <$> p <*> rest
  where
    rest = (f <$> op <*> rest) <|> pReturn id
    f o os a = os (o a)

-- TODO parentheses

-- need a `ternaryL` function for left-associative
ternaryR :: (a -> a -> a -> a) -> Parser t b -> Parser t c -> Parser t a -> Parser t a
ternaryR f op1 op2 p = (g <$> p <*> op1 <*> recur <*> op2 <*> recur) <|> p -- TODO factor out the `p`
  where
    recur = ternaryR f op1 op2 p
    g a1 _ a2 _ a3 = f a1 a2 a3

-- TODO "if ... then ... else ..."
-- TODO x.y, x[y], lambda x: ..., 

pPrefix = prefix (One <$> (pChoice $ map pSyms ["!", "~", "-", "+"])) (Leaf <$> pInteger)

pPostfix = postfix (OnePost <$> (pChoice $ map pSyms ["!", "`", "_"])) pPrefix
-- ["?", "!"])) pPrefix -- realized that postfix '?' conflicts with ternary ?:

pPrefix2 = prefix (One <$> (pChoice $ map pSyms ["$", "@", "#"])) pPostfix

pExp = chainR (Two <$> (pChoice $ map pSyms ["**", "^"])) pPrefix2

pMult = chainL (Two <$> (pChoice $ map pSyms ["*", "x", "/", "%"])) pExp

pAdd :: Parser Char (T Int String)
pAdd = chainL op p
  where
    op = Two <$> (pChoice $ map pSyms ["+", "-"])
    p = pMult

pTern = ternaryR (Three "?-:") (pSym '?') (pSym ':') pAdd


run :: Parser t a -> [t] -> Maybe ([t], a)
run = unParser

eg = run (showT <$> pTern) "$3!+4"


pRange :: Ord t => t -> t -> Parser t t
pRange low high = pSatisfy (\x -> low <= x && x <= high)

-- let's try some stuff, and maybe eventually get to python
-- (_)    -- (x)
-- _(...) -- f(x)(y)        -- { f(x) } (y)       -- how do we get this to left-associate?
-- _[...] -- obj[ix][iy]    -- { obj[ix] } [iy]
-- _._    -- obj.prop.name  -- { obj.a } .b
-- \...:_ -- lambda x: y
-- ^
-- *, /
-- +, -
-- TODO what about comma?

-- see:
--   https://docs.python.org/3/reference/grammar.html
--   https://docs.python.org/3/reference/expressions.html

data PyOp
    = PyApply PyOp [PyOp]
    | PySlot PyOp PyOp
    | PyProp PyOp PyOp
    | PyFn [String] PyOp
    | PyBinary String PyOp PyOp
    | PyPrefix String PyOp
    | PyIfThenElse PyOp PyOp PyOp
    | PyVar String
    | PyNum String
    | PyParens PyOp
    | PyTuple [PyOp]
    | PyList [PyOp]
    | PyMap [(PyOp, PyOp)]
    | PySet [PyOp]
  deriving (Show)

pyShow (PyApply f args) = "[ " ++ pyShow f ++ "( " ++ intercalate ", " (map pyShow args) ++ " ) ]"
pyShow (PySlot op arg) = "( " ++ pyShow op ++ " [" ++ pyShow arg ++ "] )"
pyShow (PyProp expr field) = "( " ++ pyShow expr ++ " . " ++ pyShow field ++ " )"
pyShow (PyFn args expr) = "( \\ " ++ concat args ++ " -> " ++ pyShow expr ++ " )"
pyShow (PyBinary op x y) = "( " ++ pyShow x ++ " " ++ op ++ " " ++ pyShow y ++ " )"
pyShow (PyPrefix op x) = "( " ++ op ++ " " ++ pyShow x ++ " )"
pyShow (PyIfThenElse a b c) = "( " ++ pyShow a ++ " if " ++ pyShow b ++ " else " ++ pyShow c ++ " )"
pyShow (PyVar s) = s
pyShow (PyNum x) = x
pyShow (PyParens x) = "( " ++ pyShow x ++ " )"
pyShow (PyTuple xs) = "( " ++ intercalate ", " (map pyShow xs) ++ ", )"
pyShow (PyList xs) = "[ " ++ intercalate ", " (map pyShow xs) ++ " ]"
pyShow (PyMap kvs) = "{ " ++ intercalate ", " (map kvShow kvs) ++ " }"
  where
    kvShow (k, v) = pyShow k ++ ": " ++ pyShow v
pyShow (PySet xs) = "{ " ++ intercalate ", " (map pyShow xs) ++ " }"

pyParens = (\_ x _ -> PyParens x) <$> pSym '(' <*> pyExpr <*> pSym ')'

pyTuple = (\_ vs _ _ -> PyTuple vs) <$> open <*> sepBy1 comma pyExpr <*> optM comma <*> close
  where
    open = pSym '('
    comma = pSym ','
    close = pSym ')'

-- TODO: incorrectly disallows [3,]
--   but watch out: [,] is illegal
pyList = (\_ vs _ -> PyList vs) <$> open <*> sepBy0 comma pyExpr <*> close
  where
    open = pSym '['
    comma = pSym ','
    close = pSym ']'

-- TODO should dictionary and set be parsed separately?  is {3, 1:2} or {3:1, 2} a parse error or runtime error? (answer: parse error)

-- TODO: incorrectly disallows {3:x,}
--   but watch out: {,} is illegal
pyMap = (\_ vs _ -> PyMap vs) <$> open <*> sepBy0 comma kvpair <*> close
  where
    open = pSym '{'
    comma = pSym ','
    close = pSym '}'
    kvpair = (\k _ v -> (k, v)) <$> pyExpr <*> pSym ':' <*> pyExpr
    
-- TODO: incorrectly disallows {3,}
--   but watch out: {,} is illegal
pySet = (\_ vs _ -> PySet vs) <$> open <*> sepBy0 comma pyExpr <*> close
  where
    open = pSym '{'
    comma = pSym ','
    close = pSym '}'
    
word = pMany1 letter
  where
    letter = pRange 'a' 'z' <|> pRange 'A' 'Z'

pyVar = PyVar <$> pCheck (\x -> x /= "lambda") word

pyNum = PyNum <$> pMany1 (pRange '0' '9')

pyAtom = pyParens <|> pyNum <|> pyVar <|> pyTuple <|> pyList <|> pyMap <|> pySet

--   TODO: also at same level: slicing -- x[y:z]
pyTrailer = postfix pfOp pyAtom
  where
    pfOp = prop <|> slot <|> apply
    prop = flip PyProp <$> (pSym '.' *> pyAtom)
    slot = flip PySlot <$> (pSym '[' *> pyExpr <* pSym ']')
    -- TODO: should the comma be parsed as an operator, with precedence?
    apply = flip PyApply <$> (pSym '(' *> sepBy0 (pSym ',') pyExpr <* pSym ')')

pyExp = chainR (PyBinary <$> pSyms "**") pyTrailer
-- TODO oops -- Python has a funny rule: prefix +, -, ~ have higher precedence when on the right side of **

pySigns = prefix (PyPrefix <$> op) pyExp
  where
    op = pChoice $ map pSyms ["+", "-", "~"]

pyMult = chainL (PyBinary <$> op) pySigns
  where
    op = pChoice $ map pSyms ["*", "/", "//", "%"]

pyAdd = chainL (PyBinary <$> op) pyMult
  where
    op = pChoice $ map pSyms ["+", "-"]

pyShift = chainL (PyBinary <$> op) pyAdd
  where
    op = pChoice $ map pSyms [">>", "<<"]

pyAnd = chainL (PyBinary <$> pSyms "&") pyShift

pyXor = chainL (PyBinary <$> pSyms "^") pyAnd

pyOr = chainL (PyBinary <$> pSyms "|") pyXor

pyComp = chainL (PyBinary <$> op) pyOr
  where
    op = pChoice $ map pSyms ["in", "not in", "is", "is not", "<", ">", "<=", ">=", "!=", "=="]

pyNot = prefix (PyPrefix <$> pSyms "not") pyComp

pyAnd' = chainL (PyBinary <$> pSyms "and") pyNot

pyOr' = chainL (PyBinary <$> pSyms "or") pyAnd'

-- TODO is this left- or right-associative?  docs seem to say left, but seems to be right
-- compare:  
{-
>>> (1 if 2 else 3) if 0 else 5
5
>>> 1 if 2 else (3 if 0 else 5)
1
>>> 1 if 2 else 3 if 0 else 5
1
-}
pyIfElse = chainR middle pyOr'
  where
    middle = (\_ b _ a c -> PyIfThenElse a b c) <$> pSyms "if" <*> pyExpr <*> pSyms "else"

pyFn = prefix middle pyIfElse
  where
    middle = (\_ names _ expr -> PyFn names expr) <$> pSyms "lambda " <*> sepBy0 (pSym ',') word <*> pSym ':'

pyExpr = pyFn

pyRun = run . (<$>) pyShow

pyEgs = map (pyRun pyExpr) ["abc[def].ghi[jkl]", "(abc[def].ghi)[jkl]", "3+4/x<<4>>z**a**b", "2**-3**2", "lambda x,y:3+lambda z:z"]
pyEgs' = map (\x -> case x of (Just y) -> snd y; _ -> "") pyEgs
pyEgs'' = mapM putStrLn pyEgs'
