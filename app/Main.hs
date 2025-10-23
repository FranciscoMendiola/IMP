-- app/Main.hs
module Main where

import Com.Syrion.Models.Automata.AFD (afnToAfd, prettyAFD)
import Com.Syrion.Models.Automata.AFDMin (afdToAfdMin, finalesM, prettyAFDmin)
import Com.Syrion.Models.Automata.AFN (afnEpToAFN, prettyAFN)
import Com.Syrion.Models.Automata.AFNEp (exprRegToAFNEp, prettyAFNEp)
import Com.Syrion.Models.Automata.MDD (MDD, afdMinToMDD, runMDD)

import Com.Syrion.Models.Lenguajes.ImpFast (scanIMP)
import Com.Syrion.Models.Lenguajes.ImpSpec (impMu)
import Com.Syrion.Models.Lexer.Token (TokenKind (..))
import Com.Syrion.Models.Regex.ExprReg (ExprReg (..), term, ($$))
import Data.List (unwords)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hSetEncoding, stdout, utf8)

-- ---------------------------------------------------------------------
-- Uso
-- ---------------------------------------------------------------------
usage :: String
usage =
  "Uso:\n"
    <> "  imp [-p | --process] [-f | --file archivo.imp] [programa IMP]\n"
    <> " Banderas:\n"
    <> "  -p/--process (opcional) debe ir antes si se usa -f.\n"
    <> "  -f/--file (opcional) debe ir inmediatamente después de -p/--process si se usa.\n"
    <> "  Si no hay -f, el resto se toma como programa en terminal.\n"

-- -------------------------------------------------------------------------------
-- parseP: detecta la bandera -p/--process y devuelve (estadoBandera, restoArgs)
-- -------------------------------------------------------------------------------
parseP :: [String] -> (Bool, [String])
parseP ("-p" : xs) = (True, xs)
parseP ("--process" : xs) = (True, xs)
parseP ("-P" : xs) = (True, xs) -- opcional
parseP xs = (False, xs)

-- ---------------------------------------------------------------------
-- parseF: detecta -f/--file en y devuelve (rutaArchivo, restoArgs)
-- ---------------------------------------------------------------------
parseF :: [String] -> (Maybe FilePath, [String])
parseF ("-f" : p : xs) = (Just p, xs)
parseF ("--file" : p : xs) = (Just p, xs)
parseF xs = (Nothing, xs)

-- ---------------------------------------------------------------------
-- Construye una ER que es la concatenación de todos los caracteres
-- del programa dado (pipeline "del programa").
-- ---------------------------------------------------------------------
erFromString :: String -> ExprReg
erFromString "" = Epsilon
erFromString (c : cs) = foldl ($$) (term c) (map term cs)

-- ---------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------
main :: IO ()
main = do
  hSetEncoding stdout utf8
  args0 <- getArgs

  -- 1) -p (si está) debe ir primero
  let (pflag, restP) = parseP args0

  -- 2) -f (si está) debe ir inmediatamente después de -p
  let (mfile, restF) = parseF restP

  -- 3) Programa: archivo o terminal
  prog <- case mfile of
    Just path -> readFile path
    Nothing ->
      let s = unwords restF
       in if null s then die usage else pure s

  -- 4) Imprimir pipeline completo (opcional)
  if pflag then demoPipeline prog else pure ()

  -- 5) Lexer IMP: imprime cada token con `show`
  putStrLn "=============================================================" 
  putStrLn "Result Lexer - IMP: Tokens"
  putStrLn "============================================================="
  case scanIMP prog of
    Left err -> putStrLn ("LEX ERROR: " ++ err)
    Right toks -> mapM_ (putStrLn . show) toks

-- ---------------------------------------------------------------------
-- Pipeline del programa:
--   ER(prog) -> AFNε -> AFN -> AFD -> AFDmin -> MDD
--   (MDD se smoke-testea con el MISMO programa)
-- ---------------------------------------------------------------------
demoPipeline :: String -> IO ()
demoPipeline prog = do
  putStrLn "== PIPELINE (del programa): ER(prog) -> AFNε -> AFN -> AFD -> AFDmin -> MDD =="

  -- ER del programa
  let er = erFromString prog

  -- Paso 1: ER -> AFN-ε
  let nfae = exprRegToAFNEp er
  putStrLn "== AFNε =="
  putStrLn (prettyAFNEp nfae)

  -- Paso 2: AFN-ε -> AFN
  let nfa = afnEpToAFN nfae
  putStrLn "== AFN =="
  putStrLn (prettyAFN nfa)

  -- Paso 3: AFN -> AFD
  let afd = afnToAfd nfa
  putStrLn "== AFD =="
  putStrLn (prettyAFD afd)

  -- Paso 4: AFD -> AFDmin
  let am = afdToAfdMin afd
  putStrLn "== AFDmin =="
  putStrLn (prettyAFDmin am)

  -- Paso 5: AFDmin -> MDD
  let mu = Map.fromList [(q, Id) | q <- finalesM am]
  let mdd :: MDD
      mdd = afdMinToMDD am mu
  putStrLn "== MDD =="
  putStrLn "(MDD construido a partir del programa.)"

  -- Ejecuta MDD sobre el mismo programa
  let clas xs = map (\(_, lx) -> (impMu lx, lx)) (runMDD mdd xs)
  putStrLn "== MDD · Clasificación (impMu) =="
  mapM_ print (clas prog)
