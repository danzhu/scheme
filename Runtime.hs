module Runtime (
  Value (..),
  Proc,
  Env,

  format,
  globals,
  eval,
  ) where

import Ast
import Lib

import Control.Monad.Except (throwError)
import Data.Map (Map)
import qualified Data.Map as Map

data Value = VProc Name Proc
           | VInt Int
           | VBool Bool
           | VString String
           | VSymbol String
           | VNull
           | VCons Value Value

type Run = Either String

type Proc = [Value] -> Run Value

type Env = Map Name Value

format :: Value -> String
format v | quo v     = '\'' : format' v
         | otherwise = format' v

format' :: Value -> String
format' (VProc (Name "") _)   = "#<procedure>"
format' (VProc (Name name) _) = "#<procedure:" ++ name ++ ">"
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
quo VProc{}   = False
quo VInt{}    = False
quo VBool{}   = False
quo VString{} = False
quo VSymbol{} = True
quo VNull{}   = True
quo VCons{}   = True

globals :: Env
globals = Map.fromList
  [ var "null"        $ VNull

  , pro "procedure?" $ cond procedure_
  , pro "integer?"   $ cond integer_
  , pro "boolean?"   $ cond boolean_
  , pro "string?"    $ cond string_
  , pro "symbol?"    $ cond symbol_
  , pro "null?"      $ cond null_
  , pro "pair?"      $ cond pair_

  , pro "apply"      $ binary apply

  , pro "+"          $ arith $ pure . sum
  , pro "-"          $ arith minus
  , pro "*"          $ arith $ pure . product
  , pro "/"          $ arith divide
  , pro "modulo"     $ arith $ binary $ (pure .) . mod
  , pro "="          $ comp (==)
  , pro "<"          $ comp (<)
  , pro "<="         $ comp (<=)
  , pro ">"          $ comp (>)
  , pro ">="         $ comp (>=)

  , pro "if"         $ ternary if_
  , pro "and"        $ logic $ pure . and
  , pro "or"         $ logic $ pure . or
  , pro "not"        $ logic $ unary $ pure . not

  , pro "cons"       $ binary $ (pure .) . VCons
  , pro "car"        $ unary car
  , pro "cdr"        $ unary cdr
  , pro "list"       $ list
  , pro "list*"      $ list_
  ]
  where var name val = (Name name, val)
        pro name fn = (Name name, VProc (Name name) fn)

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

        cond :: (Value -> Bool) -> Proc
        cond fn  = unary $ pure . VBool . fn
        arith :: ([Int] -> Run Int) -> Proc
        arith fn vs = VInt <$> (fn =<< mapM toInt vs)
        comp :: (Int -> Int -> Bool) -> Proc
        comp fn vs = VBool <$> (compList fn =<< mapM toInt vs)
        logic :: ([Bool] -> Run Bool) -> Proc
        logic fn vs = VBool <$> (fn =<< mapM toBool vs)

procedure_ :: Value -> Bool
procedure_ VProc{} = True
procedure_ _       = False

integer_ :: Value -> Bool
integer_ VInt{} = True
integer_ _      = False

boolean_ :: Value -> Bool
boolean_ VBool{} = True
boolean_ _       = False

string_ :: Value -> Bool
string_ VString{} = True
string_ _         = False

symbol_ :: Value -> Bool
symbol_ VSymbol{} = True
symbol_ _         = False

null_ :: Value -> Bool
null_ VNull{} = True
null_ _       = False

pair_ :: Value -> Bool
pair_ VCons{} = True
pair_ _       = False

apply :: Value -> Value -> Run Value
apply fn args = (=<< toList args) =<< toProc fn

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

toProc :: Value -> Run Proc
toProc (VProc _ fn) = pure fn
toProc _            = throwError "expect function argument"

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
eval env (Lambda params body) = pure $ VProc (Name "") fn
  where fn args = do args' <- zips params args `orErr` "incorrect number of arguments"
                     let env' = Map.fromList args' `Map.union` env
                     eval env' body
eval env (Apply fn args)      = (=<< mapM (eval env) args) =<< toProc =<< eval env fn
eval env (Let binds body)     = flip eval body . (`Map.union` env) . Map.fromList =<< mapM evalArg binds
  where evalArg (name, node) = do val <- eval env node
                                  pure (name, val)
eval env (Var name@(Name n))
  | Just v <- Map.lookup name env = pure v
  | otherwise                     = throwError $ "undefined identifier: " ++ n
eval env (List lst)               = list =<< mapM (eval env) lst
eval _ (LitInt val)               = pure $ VInt val
eval _ (LitBool val)              = pure $ VBool val
eval _ (LitString val)            = pure $ VString val
eval _ (LitSymbol val)            = pure $ VSymbol val
