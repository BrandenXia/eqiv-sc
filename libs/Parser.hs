{-# LANGUAGE OverloadedStrings #-}

module Parser (pAst) where

import Base
import Control.Monad.Combinators.Expr
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Expr
  = OpExpr Op [Expr]
  | VarExpr Symbol
  | ConsExpr Primitive
  deriving (Show, Eq)

data Ast
  = AExpr Expr
  deriving (Show, Eq)

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "#"
    blockComment = L.skipBlockComment "---" "---"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pVarExpr :: Parser Expr
pVarExpr =
  VarExpr
    <$> ( (:)
            <$> (letterChar <|> char '_') -- must start with a letter or underscore
            <*> many (alphaNumChar <|> char '_') -- can contain letters, digits, or underscores
        )
    <?> "variable"

pConsExpr :: Parser Expr
pConsExpr = ConsExpr . PrimNum . toRational <$> L.signed space L.scientific

parens :: Parser a -> Parser a
parens = between (char '(' >> sc) (char ')' >> sc)

pTerm :: Parser Expr
pTerm = lexeme $ choice [parens pExpr, pVarExpr, pConsExpr]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm opTable

opTable :: [[Operator Parser Expr]]
opTable =
  [ [postfix "'"],
    [funcCall],
    [binary "^"],
    [binary "*", binary "/"],
    [binary "+", binary "-"],
    [binary "==", binary "!=", binary "<", binary "<=", binary ">", binary ">="],
    [binary ","],
    [binaryAny]
  ]
  where
    binary' :: Parser Op -> Operator Parser Expr
    binary' p = InfixL $ do
      op <- lexeme p
      return $ \x y -> OpExpr op [x, y]

    binary :: Text -> Operator Parser Expr
    binary op = binary' (string op $> T.unpack op)
    binaryChar = satisfy $ flip elem ("!@#$%^&*-=+|:<>?/." :: String)
    binaryAny = binary' (some binaryChar)

    -- prefix :: Text -> Operator Parser Expr
    -- prefix op = Prefix $ do
    --   _ <- lexeme (string op)
    --   return $ \x -> OpExpr (T.unpack op) [x]

    postfix :: Text -> Operator Parser Expr
    postfix op = Postfix $ do
      _ <- lexeme (string op)
      return $ \x -> OpExpr (T.unpack op) [x]

    funcCall :: Operator Parser Expr
    funcCall = Postfix $ do
      args <- parens (pExpr `sepBy` (char ',' >> sc))
      return $ \f -> OpExpr "call" (f : args)

pAst :: Parser Ast
pAst = AExpr <$> pExpr <* eof
