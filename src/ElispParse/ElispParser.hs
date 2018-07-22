{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ElispParse.ElispParser () where
    import qualified Data.Text as T
    import Control.Monad
    import Data.Monoid
    import qualified Data.Functor.Identity
    import Data.Void
    import Data.Proxy
    import Text.Megaparsec as M
    import Text.Megaparsec.Char
    import Text.Megaparsec.Expr
    import qualified Text.Megaparsec.Char.Lexer as L

    type Parser = Parsec Void T.Text

    data ElExpr = Quote Symbol
                | Seq [ElExpr]
                | Backquote Symbol
                | Form Symbol
        deriving Show

    newtype Symbol = Symbol T.Text
        deriving Show
    newtype Identifier = Identifier T.Text
        deriving Show

    specialForms :: [T.Text]
    specialForms = ["and", "catch", "cond", "condition-case", "defconst",
        "defvar", "function", "if", "interactive", "lambda", "let", "let*",
        "or", "prog1", "prog2", "progn", "quote", "save-current-buffer",
        "save-excursion", "save-restriction", "setq", "setq-default",
        "track-mouse", "unwind-protect", "while"]

    spaceConsumer :: Parser ()
    spaceConsumer = L.space space1 lineComment blockComment
     where
        lineComment = L.skipLineComment ";"
        blockComment = pure ()
    lexeme :: forall a. Parser a -> Parser a
    lexeme = L.lexeme spaceConsumer

    symbol :: T.Text -> Parser Symbol
    symbol = fmap Symbol . L.symbol spaceConsumer

    parens :: forall a. Parser a -> Parser a
    parens = between (symbol "(") (symbol ")")

    identifier :: Parser Identifier
    identifier = Identifier <$> (lexeme  (p))
       where
        p = T.pack <$> (tokenToChunk (Proxy @String) <$> (letterChar <|> separatorChar <|> symbolChar)) |*> many alphaNumChar
        (|*>) = liftM2 (<>)
    -- identifier = undefined

    quote :: Parser ElExpr
    quote = char '\'' *> expr

    backquote :: Parser ElExpr
    backquote = char '`' *> ((string "," <|> (string $ ",@")) *> expr)

    form :: Parser ElExpr
    form = parens $ fmap (\(Identifier x) -> Form $ Symbol x) identifier

    expr :: Parser ElExpr
    -- expr = undefined
    expr = f <$> sepBy1 expr' space
      where
        f l = if length l == 1 then head l else Seq l

    expr' :: Parser ElExpr
    expr' = backquote
        <|> quote
        <|> form

    lispParser :: Parser ElExpr
    lispParser = between spaceConsumer eof expr
