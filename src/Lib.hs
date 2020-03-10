module Lib where

import Prelude hiding (sum, not)
import qualified Prelude as P

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data Expression
    = Variable String -- this, theoretically, shouldn't appear in the final product
    | Zero
    | One
    | Sum Expression Expression

data Statement
    = Equals Expression Expression
    | Congruent Int Expression Expression
    | Exists (Expression -> Statement)
    | Not Statement
    | Implies Statement Statement

type Parser = Parsec Void Text

identifier :: Parser String
identifier = many $ oneOf ['a'..'z']

variable :: Parser Expression
variable = Variable <$> identifier

zero :: Parser Expression
zero = single '0' >> return Zero

one :: Parser Expression
one = single '1' >> return One

sum :: Parser Expression
sum = do
    a <- expression
    single '+'
    b <- expression
    return $ Sum a b

expression :: Parser Expression
expression = variable <|> zero <|> one <|> sum

equals :: Parser Statement
equals = do
    single '('
    a <- expression
    single '='
    b <- expression
    single ')'
    return $ Equals a b

congruent :: Parser Statement
congruent = do
    a <- expression
    single '='
    n <- read <$> many digitChar -- TODO disallow 0 and 1
    b <- expression
    return $ Congruent n a b

not :: Parser Statement
not = do
    single 'N'
    s <- statement
    return $ Not s

implies :: Parser Statement
implies = do
    single 'C'
    s1 <- statement
    s2 <- statement
    return $ Implies s1 s2

exists :: Parser Statement
exists = do
    single 'E'
    ident <- identifier
    s <- statement
    let f = makeFunc ident s
    return $ Exists f

makeFunc :: String -> Statement -> Expression -> Statement
makeFunc ident s x = case s of
    Equals expr1 expr2 -> Equals (replaceIdent ident x expr1) (replaceIdent ident x expr2)
    Not s1 -> makeFunc ident s1 x
    Congruent n expr1 expr2 -> Congruent n (replaceIdent ident x expr1) (replaceIdent ident x expr2)
    Implies s1 s2 -> Implies (makeFunc ident s1 x) (makeFunc ident s1 x)
    Exists f -> Exists $ \y -> makeFunc ident (f y) x

replaceIdent :: String -> Expression -> Expression -> Expression
replaceIdent ident x expr = case expr of
    Variable ident' -> if ident == ident' then x else Variable ident'
    Sum expr1 expr2 -> Sum (replaceIdent ident x expr1) (replaceIdent ident x expr2)
    z -> z
    
statement :: Parser Statement
statement = equals <|> congruent <|> not <|> implies <|> exists

evalExpr :: Expression -> Int
evalExpr expr = case expr of
    Zero -> 0
    One -> 1
    Sum expr1 expr2 -> evalExpr expr1 + evalExpr expr2
    Variable _ -> error "cannot evaluate unbound identifier"

eval :: Statement -> Bool
eval s = case s of
    Equals expr1 expr2 -> evalExpr expr1 == evalExpr expr2
    Implies s1 s2 -> P.not (eval s1) || eval s2
    Not s1 -> P.not (eval s1)
    Congruent n expr1 expr2 -> (evalExpr expr1 - evalExpr expr2) `mod` n == 0
    Exists f -> undefined -- TODO This is the main insight from Pressburger's original paper
                          -- and essentially the crux of this entire excercise
