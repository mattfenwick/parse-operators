
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
    | Node b (T a b) (T a b)
  deriving (Show)

showT :: (Show a, Show b) => T a b -> String
showT (Leaf x) = show x
showT (Node op l r) = "(" ++ showT l ++ " " ++ opStr ++ " " ++ showT r ++ ")"
  where
    opStr = filter ((/=) '"') $ show op

-- pPlus = Node <$> (Leaf <$> pInteger) <* pSym '+' <*> (Leaf <$> pInteger)

flip3 f x y z = f x z y

pPlus' = applyAll <$> pInt <*> pMany0 (flip3 Node <$> (plus <|> minus) <*> pInt)
  where
    plus = pSym '+' -- *> pReturn (+)
    minus = pSym '-' -- *> pReturn (-)
    pInt = Leaf <$> pInteger

applyAll :: a -> [a -> a] -> a 
applyAll x [] = x
applyAll x (f:fs) = applyAll (f x) fs

pChainL :: Parser t a -> Parser t b -> Parser t (T b a)
pChainL op p = applyAll <$> term <*> pMany0 (flip3 Node <$> op <*> term)
  where
    term = Leaf <$> p

pPlus'' = pChainL (pChoice $ map pSym "+-") pInteger

pChainR op p = 
    p <|> 
    (flip ($) <$> p <*> (flip <$> op <*> pChainR op p))

-- pPow = pChainR (pChoice $ map pSyms ["^", "**"]) pInteger
pPow = pChainR ((pReturn (^) <* pSym '^') <|> (pReturn (^) <* pSyms "**")) pInteger

{-
tFold :: T a -> a
tFold (Leaf x) = x
tFold (Node f l r) = f (tFold l) (tFold r)
-}

{-
1 - 2 - 3
(1 - 2) - 3

4 ** 5 ** 6
4 ** (5 ** 6)

4 + 3 ** 2 ** 1 / 8 - 4 * 5
(4 + ((3 ** (2 ** 1)) / 8)) - (4 * 5)
-}

chainR op p = (f <$> p <*> op <*> chainR op p) <|> p
  where
    f a b c = b a c

chainL op p = flip ($) <$> p <*> rest
  where
    f b c rs a = rs (b a c)
    rest = (f <$> op <*> p <*> rest) <|> pReturn id

pExp = chainR (Node <$> (pChoice $ map pSyms ["**", "^"])) (Leaf <$> pInteger)

pMult = chainL (Node <$> (pChoice $ map pSyms ["*", "x", "/", "%"])) pExp

pAdd = chainL op p
  where
    op = Node <$> (pChoice $ map pSyms ["+", "-"])
    p = pMult

run :: Parser t a -> [t] -> Maybe ([t], a)
run = unParser

