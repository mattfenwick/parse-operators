
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

infixl 5 <*>
(<*>) :: Parser t (a -> b) -> Parser t a -> Parser t b
Parser p1 <*> Parser p2 = Parser (\xs -> case (p1 xs) of
                                              Nothing -> Nothing
                                              Just (ys, f) -> case (p2 ys) of
                                                                   Nothing -> Nothing
                                                                   Just (zs, x) -> Just (zs, f x))

infixr 3 <|>
(<|>) :: Parser t a -> Parser t a -> Parser t a
Parser p1 <|> Parser p2 = Parser (\xs -> let r1 = p1 xs in case r1 of 
                                                                Nothing -> p2 xs
                                                                _ -> r1)

pChoice :: [Parser t a] -> Parser t a
pChoice = foldr (<|>) pFail

infix 7 <$>
(<$>) :: (a -> b) -> Parser t a -> Parser t b
f <$> Parser p = Parser (\xs -> case (p xs) of 
                                     Just (ys, r) -> Just (ys, f r)
                                     _ -> Nothing)

opt :: Parser t a -> a -> Parser t a
opt p x = p <|> pReturn x

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
    g a1 s1 a2 s2 a3 = f a1 a2 a3

-- TODO "if ... then ... else ..."
-- TODO x.y, x[y], lambda x: ..., 

-- examples
-- see:
--   https://docs.python.org/3/reference/grammar.html
--   https://docs.python.org/3/reference/expressions.html

pPrefix = prefix (One <$> (pChoice $ map pSyms ["!", "~", "-", "+"])) (Leaf <$> pInteger)

pPostfix = postfix (OnePost <$> (pChoice $ map pSyms ["?", "!"])) pPrefix

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

