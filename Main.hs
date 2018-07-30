import Ast
import Parser
import Runtime

import Control.Monad (forM_)
import qualified System.Environment as Env

main :: IO ()
main = do args <- Env.getArgs
          src <- case args of
            (file : _) -> readFile file
            _          -> getContents
          case parse lang src of
            Nothing         -> putStrLn "parse error"
            Just (nodes, _) -> forM_ nodes $ \node ->
              case eval globals node of
                Left err  -> putStrLn $ "error: " ++ err
                Right val -> putStrLn $ format val
