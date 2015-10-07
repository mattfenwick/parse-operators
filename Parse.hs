module Parse (

    Parser(..)
  
  , pSym
  , pReturn
  , pFail
  , (>>=)
  , (>>)
  , (<*>)
  , (<|>)
  , pChoice
  , pCheck
  , (<$>)
  , opt
  , optM
  , (<*)
  , (*>)
  , (<$)
  , pSyms
  , pMany0
  , pMany1
  , pSatisfy
  , chainR
  , chainL
  , sepBy0
  , sepBy1
  , prefix
  , postfix
  , ternaryR

) where

import Prelude hiding ((<$>), (<*>), (*>), (<*), (>>=), (>>), (<$))

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

pMany0 :: Parser t a -> Parser t [a]
pMany0 p = opt ((:) <$> p <*> pMany0 p) []

pMany1 :: Parser t a -> Parser t [a]
pMany1 p = (:) <$> p <*> pMany0 p

pSatisfy :: (t -> Bool) -> Parser t t
pSatisfy pred = Parser (\xs -> case xs of
                                    (y:ys)
                                      | pred y -> Just (ys, y)
                                    _ -> Nothing)

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

-- need a `ternaryL` function for left-associative
ternaryR :: (a -> a -> a -> a) -> Parser t b -> Parser t c -> Parser t a -> Parser t a
ternaryR f op1 op2 p = (g <$> p <*> op1 <*> recur <*> op2 <*> recur) <|> p -- TODO factor out the `p`
  where
    recur = ternaryR f op1 op2 p
    g a1 _ a2 _ a3 = f a1 a2 a3
