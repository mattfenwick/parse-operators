import Parse
import Prelude hiding ((<$>), (<*>), (*>), (<*), (>>=), (>>), (<$))
import Data.List (intercalate)


{-
pParens :: Parser Char a -> Parser Char a
pParens p = id <$ pSym '(' <*> p <* pSym ')'

parens = (max . (1 +)) <$> pParens parens <*> opt parens 0
-}


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


pTern = ternaryR (Three "?-:") (pSym '?') (pSym ':') pAdd


run :: Parser t a -> [t] -> Maybe ([t], a)
run = unParser

eg = run (showT <$> pTern) "$3!+4"

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
