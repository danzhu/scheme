import Control.Applicative hiding (many, optional)
import System.Environment

-- parser
newtype Parser a = Parser (String -> Either ParseError (a, String))

instance Functor Parser where
  fmap f (Parser p) = Parser par
    where par s = do (value, s') <- p s
                     Right (f value, s')

instance Applicative Parser where
  pure x = Parser par
    where par s = Right (x, s)

  (Parser p) <*> (Parser q) = Parser par
    where par s = do (f, s') <- p s
                     (value, s'') <- q s'
                     Right (f value, s'')

instance Monad Parser where
  return = pure

  (Parser p) >>= f = Parser par
    where par s = do (value, s') <- p s
                     let (Parser p') = f value
                     p' s'

instance Alternative Parser where
  empty = Parser par
    where par _ = Left Failed

  (Parser p) <|> (Parser q) = Parser par
    where par s
            | Right res <- p s = Right res
            | otherwise        = q s

data ParseError = Expect String
                | Failed

instance Show ParseError where
  show (Expect s) = "expect " ++ s
  show Failed     = "parse failed"

parse :: Parser a -> String -> Either ParseError a
parse (Parser p) = fmap fst . p

-- primitives
anything :: Parser Char
anything = Parser par
  where par (c:s) = Right (c, s)
        par []    = Left $ Expect "anything"

char :: Char -> Parser ()
char ch = Parser par
  where par (c:s)
          | c == ch = Right ((), s)
        par _       = Left $ Expect $ "charater: " ++ [ch]

keyword :: String -> Parser ()
keyword value = Parser $ par value
  where par (ch:str) (c:s)
          | c == ch = par str s
        par [] s    = Right ((), s)
        par str _   = Left $ Expect $ "keyword: " ++ str

oneOf :: [Char] -> Parser Char
oneOf chars = Parser par
  where par (c:s)
          | c `elem` chars = Right (c, s)
        par _              = Left $ Expect $ "one of: " ++ chars


noneOf :: [Char] -> Parser Char
noneOf chars = Parser par
  where par (c:s)
          | not $ c `elem` chars = Right (c, s)
        par _                    = Left $ Expect $ "none of: " ++ chars

many :: Parser a -> Parser [a]
many (Parser p) = Parser $ Right . par
  where par s
          | Right (v, s') <- p s = let (vs, s'') = par s'
                                   in (v : vs, s'')
          | otherwise            = ([], s)

eof :: Parser ()
eof = Parser par
  where par "" = Right ((), [])
        par _  = Left $ Expect "EOF"

-- combinators
skip :: Parser a -> Parser ()
skip p = p >> return ()

optional :: Parser a -> Parser (Maybe a)
optional p = fmap Just p <|> return Nothing

many1 :: Parser a -> Parser [a]
many1 p = do value <- p
             values <- many p
             return $ value : values

-- lang
data Node = Lambda [String] Node
          | Apply Node [Node]
          | Let [(String, Node)] Node
          | Var String
          | List [Node]
          | Literal Value

data Value = Func String Func
           | Int Int
           | Bool Bool
           | String String
           | Symbol String
           | Null
           | Cons Value Value

instance Show Value where
  show f@(Func _ _) = showQuoted f
  show i@(Int _)    = showQuoted i
  show b@(Bool _)   = showQuoted b
  show s@(String _) = showQuoted s
  show s@(Symbol _) = '\'' : showQuoted s
  show e@Null       = '\'' : showQuoted e
  show c@(Cons _ _) = '\'' : showQuoted c

type Func = [Value] -> Value

type Env = [(String, Value)]

showQuoted :: Value -> String
showQuoted (Func "" _)   = "#<procedure>"
showQuoted (Func name _) = "#<procedure:" ++ name ++ ">"
showQuoted (Int n)       = show n
showQuoted (Bool True)   = "#t"
showQuoted (Bool False)  = "#f"
showQuoted (String s)    = show s
showQuoted (Symbol s)    = s
showQuoted Null          = "()"
showQuoted (Cons v vs)   = "(" ++ showQuoted v ++ showRest vs ++ ")"
  where showRest Null        = ""
        showRest (Cons x xs) = " " ++ showQuoted x ++ showRest xs
        showRest xs          = " . " ++ showQuoted xs

-- parsing
spaces :: Parser ()
spaces = skip $ many $ space <|> comment
  where space   = skip $ oneOf " \t\r\n"
        comment = do char ';'
                     skip $ many $ noneOf "\n"

open :: Parser ()
open = spaces >> char '('

close :: Parser ()
close = spaces >> char ')'

ident :: Parser String
ident = do spaces
           start <- oneOf identStart
           value <- many $ oneOf identChars
           return $ start : value
  where identStart = ['a'..'z'] ++ ['A'..'Z'] ++ "$%&*+-/<=>?"
        identChars = identStart ++ ['0'..'9']

number :: Parser Node
number = do spaces
            sign <- optional $ char '-'
            value <- many1 $ oneOf ['0'..'9']
            let num = case sign of
                  Just () -> '-' : value
                  Nothing -> value
            return $ Literal $ Int $ read num

boolean :: Parser Node
boolean = do spaces
             char '#'
             value <- oneOf "tf"
             let bool = case value of
                   't' -> True
                   'f' -> False
                   _   -> error "unreachable"
             return $ Literal $ Bool bool

string :: Parser Node
string = do spaces >> char '"'
            cont <- many $ escape <|> (fmap (:[]) $ noneOf "\"")
            char '"'
            let value = "\"" ++ concat cont ++ "\""
            return $ Literal $ String $ read value
  where escape = do char '\\'
                    value <- anything
                    return ['\\', value]

quoted :: Parser Node
quoted = string <|> boolean <|> number <|> symbol <|> lst
  where symbol = fmap (Literal . Symbol) ident
        lst    = do open
                    values <- many quoted
                    close
                    return $ List values

expr :: Parser Node
expr = let_ <|> lambda <|> call <|> quote <|> string <|> boolean <|> number <|> var
  where var    = fmap Var ident
        quote  = do spaces >> char '\''
                    quoted
        call   = do open
                    fn:args <- many1 expr
                    close
                    return $ Apply fn args
        lambda = do open
                    spaces >> keyword "lambda"
                    open
                    params <- many ident
                    close
                    body <- expr
                    close
                    return $ Lambda params body
        bind   = do open
                    name <- ident
                    value <- expr
                    close
                    return (name, value)
        let_   = do open
                    spaces >> keyword "let"
                    open
                    binds <- many bind
                    close
                    body <- expr
                    close
                    return $ Let binds body

lang :: Parser [Node]
lang = do exprs <- many expr
          spaces
          eof
          return exprs

-- runtime
globals :: Env
globals = [ var "null"       $ Null

          , func "function?" $ cond func_
          , func "integer?"  $ cond integer_
          , func "boolean?"  $ cond boolean_
          , func "string?"   $ cond string_
          , func "symbol?"   $ cond symbol_
          , func "null?"     $ cond null_
          , func "pair?"     $ cond pair_

          , func "apply"     $ binary apply

          , func "+"         $ arith sum
          , func "-"         $ arith minus
          , func "*"         $ arith product
          , func "/"         $ arith divide
          , func "modulo"    $ arith $ binary mod
          , func "="         $ comp (==)
          , func "<"         $ comp (<)
          , func "<="        $ comp (<=)
          , func ">"         $ comp (>)
          , func ">="        $ comp (>=)

          , func "if"        $ ternary if_
          , func "and"       $ logic and
          , func "or"        $ logic or
          , func "not"       $ logic $ unary not

          , func "cons"      $ binary Cons
          , func "car"       $ unary car
          , func "cdr"       $ unary cdr
          , func "list"      $ list
          , func "list*"     $ list_
          ]

  where func name fn = (name, Func name fn)
        var name val = (name, val)

        unary fn [a]         = fn a
        unary _ _            = error "expect 1 argument"
        binary fn [a, b]     = fn a b
        binary _ _           = error "expect 2 arguments"
        ternary fn [a, b, c] = fn a b c
        ternary _ _          = error "expect 3 arguments"

        compList fn [a, b]       = fn a b
        compList fn (a:as@(b:_)) = fn a b && compList fn as
        compList _ _             = error "expect at least 2 arguments"

        cond fn  = Bool . unary fn
        arith fn = Int . fn . map toInt
        comp fn  = Bool . compList fn . map toInt
        logic fn = Bool . fn . map toBool

func_ :: Value -> Bool
func_ (Func _ _) = True
func_ _          = False

integer_ :: Value -> Bool
integer_ (Int _) = True
integer_ _       = False

boolean_ :: Value -> Bool
boolean_ (Bool _) = True
boolean_ _        = False

string_ :: Value -> Bool
string_ (String _) = True
string_ _          = False

symbol_ :: Value -> Bool
symbol_ (Symbol _) = True
symbol_ _          = False

null_ :: Value -> Bool
null_ Null = True
null_ _    = False

pair_ :: Value -> Bool
pair_ (Cons _ _) = True
pair_ _          = False

apply :: Value -> Value -> Value
apply fn args = toFunc fn $ toList args

minus :: [Int] -> Int
minus [i]    = -i
minus (i:is) = i - sum is
minus []     = error "expect at least 1 argument"

divide :: [Int] -> Int
divide [i]    = 1 `div` i
divide (i:is) = i `div` product is
divide []     = error "expect at least 1 argument"

if_ :: Value -> Value -> Value -> Value
if_ b t f = if toBool b then t else f

car :: Value -> Value
car (Cons v _) = v
car _          = error "expect cons argument"

cdr :: Value -> Value
cdr (Cons _ v) = v
cdr _          = error "expect cons argument"

list :: [Value] -> Value
list []     = Null
list (v:vs) = Cons v $ list vs

list_ :: [Value] -> Value
list_ [v]    = v
list_ (v:vs) = Cons v $ list_ vs
list_ []     = error "expect at least 1 argument"

toFunc :: Value -> Func
toFunc (Func _ fn) = fn
toFunc _           = error "expect function argument"

toInt :: Value -> Int
toInt (Int i) = i
toInt _       = error "expect integer argument"

toBool :: Value -> Bool
toBool (Bool b) = b
toBool _        = error "expect boolean argument"

toList :: Value -> [Value]
toList Null        = []
toList (Cons v vs) = v : toList vs
toList _           = error "expect list argument"

eval :: Env -> Node -> Value
eval env (Lambda params body) = Func "" $ \args -> eval (bind params args) body
  where bind [] []              = env
        bind (name:xs) (val:ys) = (name, val) : bind xs ys
        bind _ _                = error "incorrect number of arguments"
eval env (Apply fn args)
  | Func _ f <- eval env fn   = f $ map (eval env) args
  | otherwise                 = error "application requires function type"
eval env (Let binds body)     = eval (bind binds) body
  where bind []                = env
        bind ((name, node):bs) = (name, eval env node) : bind bs
eval env (Var name)
  | Just v <- lookup name env = v
  | otherwise                 = error $ "undefined identifier: " ++ name
eval env (List lst)           = list $ map (eval env) lst
eval _ (Literal val)          = val

main :: IO ()
main = do args <- getArgs
          src <- case args of
            (file:_) -> readFile file
            _        -> getContents
          case parse lang src of
            Right nodes -> mapM_ (print . eval globals) nodes
            Left err    -> do putStr "error: "
                              print err
