{-# LANGUAGE OverloadedStrings #-}

module Parser (pExpr) where

import Base
import Data.Char
import Data.Text (Text)
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

pConsExpr :: Parser Expr
pConsExpr = ConsExpr . PrimNum . toRational <$> L.signed space L.scientific

betweenParens :: Parser a -> Parser a
betweenParens = between (char '(' >> sc) (char ')' >> sc)

pOpExpr :: Parser Expr
pOpExpr = lexeme $ choice [pBinary]
  where
    opChar = satisfy (\c -> not (isSpace c) && not (isAlphaNum c) && c /= '(' && c /= ')' && c /= '#')
    pBinary = do
      lhs <- pExpr
      _ <- space
      op <- some opChar
      rhs <- pExpr
      return $ OpExpr op [lhs, rhs]
    pNonBinary = do
      op <- some opChar
      _ <- space
      args <- lexeme $ betweenParens (sepBy pExpr $ char ',' >> sc)
      return $ OpExpr op args

pExpr :: Parser Expr
pExpr = choice [pOpExpr, pConsExpr, pVarExpr]
