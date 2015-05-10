
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

data T a
    = Leaf a
    | Node (T a) (T a)
  deriving (Show)

pPlus = Node <$> (Leaf <$> pInteger) <* pSym '+' <*> (Leaf <$> pInteger)

pPlus' = applyAll <$> pInt <*> pMany0 (flip Node <$ pSym '+' <*> pInt)
  where
    pInt = Leaf <$> pInteger
    applyAll x [] = x
    applyAll x (f:fs) = applyAll (f x) fs

run :: Parser t a -> [t] -> Maybe ([t], a)
run = unParser

