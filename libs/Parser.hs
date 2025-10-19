{-# LANGUAGE OverloadedStrings #-}

module Parser (Expr (..), Ast (..), parseAst) where

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
  deriving (Eq)

instance Show Expr where
  show (OpExpr op args) = "(" ++ op ++ " " ++ unwords (map show args) ++ ")"
  show (VarExpr v) = v
  show (ConsExpr (PrimNum n)) = show n

data Ast
  = AExpr Expr
  | ARewrite Expr Expr
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
      void $ lexeme (string op)
      return $ \x -> OpExpr (T.unpack op) [x]

    funcCall :: Operator Parser Expr
    funcCall = Postfix $ do
      args <- parens (pExpr `sepBy` (char ',' >> sc))
      return $ \f -> OpExpr "call" (f : args)

pRewrite :: Parser Ast
pRewrite = do
  void $ string "rewrite"
  void sc
  lhs <- pExpr
  void sc
  rhs <- pExpr
  return $ ARewrite lhs rhs

pAst :: Parser Ast
pAst = choice [pRewrite, AExpr <$> pExpr]

pAsts :: Parser [Ast]
pAsts = sc >> many (pAst <* sc) <* eof

parseAst :: String -> String -> Either String [Ast]
parseAst source input =
  case runParser pAsts source (T.pack input) of
    Left err -> Left $ errorBundlePretty err
    Right ast -> Right ast
