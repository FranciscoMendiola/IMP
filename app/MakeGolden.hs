-- app/MakeGolden.hs
module Main (main) where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (die)

import Com.Syrion.Models.Lenguajes.ImpFast (scanIMP)  -- ← FAST
import Com.Syrion.Models.Lexer.Lexer (Token)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      src <- T.readFile path
      case scanIMP (T.unpack src) of
        Left err    -> die ("Error de lexer: " ++ err)
        Right toks  -> mapM_ (putStrLn . show) toks
    _ -> die "Uso: make-golden <archivo.imp>"
