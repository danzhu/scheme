module Runtime (
  Value (..),
  Func,
  Env,

  format,
  globals,
  eval,
  ) where

import Ast

import Control.Monad.Except (throwError)

data Value = VFunc Name Func
           | VInt Int
           | VBool Bool
           | VString String
           | VSymbol String
           | VNull
           | VCons Value Value

type Run = Either String

type Func = [Value] -> Run Value

type Env = [(Name, Value)]

format :: Value -> String
format v | quo v     = '\'' : format' v
         | otherwise = format' v

format' :: Value -> String
format' (VFunc (Name "") _)   = "#<procedure>"
format' (VFunc (Name name) _) = "#<procedure:" ++ name ++ ">"
format' (VInt n)              = show n
format' (VBool True)          = "#t"
format' (VBool False)         = "#f"
format' (VString s)           = show s
format' (VSymbol s)           = s
format' (VNull)               = "()"
format' (VCons v vs)          = "(" ++ format' v ++ showRest vs ++ ")"
  where showRest VNull        = ""
        showRest (VCons x xs) = " " ++ format' x ++ showRest xs
        showRest xs           = " . " ++ format' xs

quo :: Value -> Bool
quo VFunc{}   = False
quo VInt{}    = False
quo VBool{}   = False
quo VString{} = False
quo VSymbol{} = True
quo VNull{}   = True
quo VCons{}   = True

globals :: Env
globals = [ var "null"       $ VNull

          , func "function?" $ cond func_
          , func "integer?"  $ cond integer_
          , func "boolean?"  $ cond boolean_
          , func "string?"   $ cond string_
          , func "symbol?"   $ cond symbol_
          , func "null?"     $ cond null_
          , func "pair?"     $ cond pair_

          , func "apply"     $ binary apply

          , func "+"         $ arith $ pure . sum
          , func "-"         $ arith minus
          , func "*"         $ arith $ pure . product
          , func "/"         $ arith divide
          , func "modulo"    $ arith $ binary $ (pure .) . mod
          , func "="         $ comp (==)
          , func "<"         $ comp (<)
          , func "<="        $ comp (<=)
          , func ">"         $ comp (>)
          , func ">="        $ comp (>=)

          , func "if"        $ ternary if_
          , func "and"       $ logic $ pure . and
          , func "or"        $ logic $ pure . or
          , func "not"       $ logic $ unary $ pure . not

          , func "cons"      $ binary $ (pure .) . VCons
          , func "car"       $ unary car
          , func "cdr"       $ unary cdr
          , func "list"      $ list
          , func "list*"     $ list_
          ]
  where func name fn = (Name name, VFunc (Name name) fn)
        var name val = (Name name, val)

        unary :: (a -> Run b) -> [a] -> Run b
        unary fn [a]         = fn a
        unary _ _            = throwError "expect 1 argument"
        binary :: (a -> a -> Run b) -> [a] -> Run b
        binary fn [a, b]     = fn a b
        binary _ _           = throwError "expect 2 arguments"
        ternary :: (a -> a -> a -> Run b) -> [a] -> Run b
        ternary fn [a, b, c] = fn a b c
        ternary _ _          = throwError "expect 3 arguments"

        compList :: (a -> a -> Bool) -> [a] -> Run Bool
        compList fn [a, b]           = pure $ fn a b
        compList fn (a : as@(b : _)) = (fn a b &&) <$> compList fn as
        compList _ _                 = throwError "expect at least 2 arguments"

        cond :: (Value -> Bool) -> Func
        cond fn  = unary $ pure . VBool . fn
        arith :: ([Int] -> Run Int) -> Func
        arith fn vs = VInt <$> (fn =<< mapM toInt vs)
        comp :: (Int -> Int -> Bool) -> Func
        comp fn vs = VBool <$> (compList fn =<< mapM toInt vs)
        logic :: ([Bool] -> Run Bool) -> Func
        logic fn vs = VBool <$> (fn =<< mapM toBool vs)

func_ :: Value -> Bool
func_ (VFunc _ _) = True
func_ _           = False

integer_ :: Value -> Bool
integer_ (VInt _) = True
integer_ _        = False

boolean_ :: Value -> Bool
boolean_ (VBool _) = True
boolean_ _         = False

string_ :: Value -> Bool
string_ (VString _) = True
string_ _           = False

symbol_ :: Value -> Bool
symbol_ (VSymbol _) = True
symbol_ _           = False

null_ :: Value -> Bool
null_ VNull = True
null_ _     = False

pair_ :: Value -> Bool
pair_ (VCons _ _) = True
pair_ _           = False

apply :: Value -> Value -> Run Value
apply fn args = do fn' <- toFunc fn
                   args' <- toList args
                   fn' args'

minus :: [Int] -> Run Int
minus [i]      = pure $ -i
minus (i : is) = pure $ i - sum is
minus []       = throwError "expect at least 1 argument"

divide :: [Int] -> Run Int
divide [i]      = pure $ 1 `div` i
divide (i : is) = pure $ i `div` product is
divide []       = throwError "expect at least 1 argument"

if_ :: Value -> Value -> Value -> Run Value
if_ b t f = do v <- toBool b
               pure $ if v then t else f

car :: Value -> Run Value
car (VCons v _) = pure v
car _           = throwError "expect cons argument"

cdr :: Value -> Run Value
cdr (VCons _ v) = pure v
cdr _           = throwError "expect cons argument"

list :: [Value] -> Run Value
list []       = pure VNull
list (v : vs) = VCons v <$> list vs

list_ :: [Value] -> Run Value
list_ [v]      = pure v
list_ (v : vs) = VCons v <$> list_ vs
list_ []       = throwError "expect at least 1 argument"

toFunc :: Value -> Run Func
toFunc (VFunc _ fn) = pure fn
toFunc _            = throwError "expect function argument"

toInt :: Value -> Run Int
toInt (VInt i) = pure i
toInt _        = throwError "expect integer argument"

toBool :: Value -> Run Bool
toBool (VBool b) = pure b
toBool _         = throwError "expect boolean argument"

toList :: Value -> Run [Value]
toList VNull        = pure []
toList (VCons v vs) = (v :) <$> toList vs
toList _            = throwError "expect list argument"

eval :: Env -> Node -> Run Value
eval env (Lambda params body) = pure $ VFunc (Name "") fn
  where bind :: [Name] -> [Value] -> Run [(Name, Value)]
        bind [] []             = pure env
        bind (x : xs) (y : ys) = do rest <- bind xs ys
                                    pure $ (x, y) : rest
        bind _ _               = throwError "incorrect number of arguments"
        fn args = flip eval body =<< bind params args
eval env (Apply fn args)      = do res <- eval env fn
                                   case res of
                                     VFunc _ f -> f =<< mapM (eval env) args
                                     _         -> error "application requires function type"
eval env (Let binds body)     = flip eval body =<< (++ env) <$> mapM evalArg binds
  where evalArg (name, node) = do val <- eval env node
                                  pure (name, val)
eval env (Var name@(Name n))
  | Just v <- lookup name env = pure v
  | otherwise                 = throwError $ "undefined identifier: " ++ n
eval env (List lst)           = list =<< mapM (eval env) lst
eval _ (LitInt val)           = pure $ VInt val
eval _ (LitBool val)          = pure $ VBool val
eval _ (LitString val)        = pure $ VString val
eval _ (LitSymbol val)        = pure $ VSymbol val
