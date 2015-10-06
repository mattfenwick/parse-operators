import Parse
import Prelude hiding ((<$>), (<*>), (*>), (<*), (>>=), (>>), (<$))
import Data.List (intercalate)

pRange :: Ord t => t -> t -> Parser t t
pRange low high = pSatisfy (\x -> low <= x && x <= high)

chomp :: Parser Char a -> Parser Char a
chomp p = p <* pMany0 (pSym ' ')

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

pyAtom = chomp (pyParens <|> pyNum <|> pyVar <|> pyTuple <|> pyList <|> pyMap <|> pySet)

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
-- TODO why isn't this parsed using 'ternaryR'?
pyIfElse = chainR middle pyOr'
  where
    middle = (\_ b _ a c -> PyIfThenElse a b c) <$> chomp (pSyms "if") <*> pyExpr <*> chomp (pSyms "else")

pyFn = prefix middle pyIfElse
  where
    middle = (\_ names _ expr -> PyFn names expr) <$> chomp (pSyms "lambda") <*> sepBy0 (pSym ',') word <*> pSym ':'

pyExpr = chomp pyFn

pyRun = unParser . (<$>) pyShow

pyEgs = map (pyRun pyExpr) ["abc[def].ghi[jkl]", 
                            "(abc[def].ghi)[jkl]", 
                            "3+4/x<<4>>z**a**b", 
                            "2**-3**2", 
                            "lambda x,y:3+lambda z:z",
                            "y.z[a]", -- should do: (y.z)[a], not y.(z[a])
                            "1 if 2 else 3 if 0 else 5", -- I think it's parsed as "1 if 2 else (3 if 0 else 5)", not "(1 if 2 else 3) if 0 else 5"
                            "3not in4" -- TODO we need to allow spaces
                            ]
pyEgs' = map (\x -> case x of (Just y) -> snd y; _ -> "") pyEgs
pyEgs'' = mapM putStrLn pyEgs'
