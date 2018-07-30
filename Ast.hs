module Ast (
  Name (..),
  Node (..),

  lang,
  ) where

import Parser

import Control.Applicative ((<|>), many, optional, liftA2)
import Control.Monad (void)

newtype Name = Name String
             deriving (Eq)

data Node = Lambda [Name] Node
          | Apply Node [Node]
          | Let [(Name, Node)] Node
          | Var Name
          | List [Node]
          | LitInt Int
          | LitBool Bool
          | LitString String
          | LitSymbol String

-- parsing
spaces :: Parser String ()
spaces = void $ many $ space <|> comment
  where space   = void $ oneOf " \t\r\n"
        comment = expect ';' >> (void $ many $ noneOf "\n")

paren :: Parser String a -> Parser String a
paren par = open *> par <* close
  where open = spaces >> expect '('
        close = spaces >> expect ')'


ident :: Parser String String
ident = spaces >> liftA2 (:) (oneOf identStart) (many $ oneOf identChars)
  where identStart = ['a'..'z'] ++ ['A'..'Z'] ++ "$%&*+-/<=>?"
        identChars = identStart ++ ['0'..'9']

number :: Parser String Node
number = do spaces
            sign <- optional $ expect '-'
            value <- many1 $ oneOf ['0'..'9']
            let num = case sign of
                  Just () -> '-' : value
                  Nothing -> value
            return $ LitInt $ read num

boolean :: Parser String Node
boolean = spaces >> expect '#' >> LitBool . bool <$> oneOf "tf"
  where bool 't' = True
        bool 'f' = False
        bool _   = error "unreachable"

string :: Parser String Node
string = do spaces
            expect '"'
            cont <- many $ escape <|> ((:[]) <$> noneOf "\"")
            expect '"'
            let value = "\"" ++ concat cont ++ "\""
            return $ LitString $ read value
  where escape = do expect '\\'
                    value <- advance
                    return ['\\', value]

quoted :: Parser String Node
quoted = string <|> boolean <|> number <|> symbol <|> lst
  where symbol = LitSymbol <$> ident
        lst    = List <$> (paren $ many quoted)

name :: Parser String Name
name = Name <$> ident

expression :: Parser String Node
expression = paren (let_ <|> lambda <|> call) <|> quote <|> string <|> boolean <|> number <|> var
  where var    = Var <$> name
        quote  = spaces >> expect '\'' >> quoted
        call   = liftA2 Apply expression $ many expression
        lambda = spaces >> expects "lambda" >> liftA2 Lambda (paren $ many name) expression
        let_   = spaces >> expects "let" >> liftA2 Let (paren $ many param) expression
        param  = paren $ liftA2 (,) name expression

lang :: Parser String [Node]
lang = many expression <* spaces <* eof
