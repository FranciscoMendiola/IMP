-- app/Main.hs
module Main where

import System.IO (hSetEncoding, stdout, utf8)
import qualified Data.Map.Strict as Map

-- ===== Parte 1: Pipeline clásico (demo ER simple) =====
import Com.Syrion.Models.Regex.ExprReg
  ( ExprReg(..), term, ($$) )  

import Com.Syrion.Models.Automata.AFNEp
  ( exprRegToAFNEp, prettyAFNEp )
import Com.Syrion.Models.Automata.AFN
  ( afnEpToAFN, prettyAFN )
import Com.Syrion.Models.Automata.AFD
  ( afnToAfd, prettyAFD )
import Com.Syrion.Models.Automata.AFDMin
  ( afdToAfdMin, prettyAFDmin, finalesM )

import Com.Syrion.Models.Automata.MDD
  ( MDD, afdMinToMDD, runMDD )

import Com.Syrion.Models.Lexer.Token  (TokenKind(..))

import Com.Syrion.Models.Lenguajes.ImpFast (scanIMP, renderTokens)
import Com.Syrion.Models.Lenguajes.ImpSpec (impMu)

imprimirPar :: (Maybe TokenKind, String) -> IO ()
imprimirPar (Just tok, lx) = putStrLn $ "    " ++ rellenarDerecha 12 (show tok) ++ " => " ++ show lx
imprimirPar (Nothing , lx) = putStrLn $ "    ERROR        => " ++ show lx

rellenarDerecha :: Int -> String -> String
rellenarDerecha n s = s ++ replicate (max 0 (n - length s)) ' '

main :: IO ()
main = do
  -- UTF-8 para Windows
  hSetEncoding stdout utf8

  putStrLn "=========================================="
  putStrLn "  ANALIZADOR LEXICO - PIPELINE COMPLETO"
  putStrLn "  ER -> AFNeps -> AFN -> AFD -> AFDmin -> MDD -> LEXER"
  putStrLn "=========================================="
  putStrLn ""

  -- ============================================
  -- PARTE 1: Pipeline completo con ER simple (ab)
  -- ============================================
  putStrLn "============================================"
  putStrLn "PARTE 1: Pipeline completo con ER simple"
  putStrLn "============================================"
  putStrLn ""

  putStrLn "--- ER: a concatenado con b (ab) ---"
  let er :: ExprReg
      er = (term 'a') $$ (term 'b')
  putStrLn $ "Expresion Regular: " ++ show er
  putStrLn ""

  -- Paso 1: ER -> AFN-ε (Thompson)
  let nfae = exprRegToAFNEp er
  putStrLn "== Paso 1: AFN-eps (Thompson) =="
  putStrLn (prettyAFNEp nfae)

  -- Paso 2: AFN-ε -> AFN (sin ε)
  let nfa = afnEpToAFN nfae
  putStrLn "== Paso 2: AFN (sin transiciones epsilon) =="
  putStrLn (prettyAFN nfa)

  -- Paso 3: AFN -> AFD (determinista)
  let afd = afnToAfd nfa
  putStrLn "== Paso 3: AFD (determinista) =="
  putStrLn (prettyAFD afd)

  -- Paso 4: AFD -> AFDmin (mínimo)
  let afdMin = afdToAfdMin afd
  putStrLn "== Paso 4: AFDmin (minimizado) =="
  putStrLn (prettyAFDmin afdMin)

  -- Paso 5: AFDmin -> MDD
  let mu = Map.fromList [ (q, Id) | q <- finalesM afdMin ]
  let mdd :: MDD
      mdd = afdMinToMDD afdMin mu
  putStrLn "== Paso 5: MDD (Maquina Discriminadora Determinista) =="
  putStrLn "MDD construido exitosamente"
  putStrLn ""

  -- Paso 6: MDD -> LEXER (aplicando funcion mu) con el MISMO estilo (sin tablas)
  putStrLn "== Paso 6: LEXER (MDD + funcion mu) =="
  putStrLn "Tokenizando cadenas de prueba:"
  putStrLn ""

  -- Usamos runMDD + impMu, y mostramos pares (Maybe TokenKind, String) con imprimirPar

  -- Prueba 1: "a" (incompleto)
  putStrLn "  Entrada: \"a\""
  let pares1 = runMDD mdd "a"
  let clasificados1 = map (\(_, lx) -> (impMu lx, lx)) pares1
  putStrLn "  Tokens:"
  mapM_ imprimirPar clasificados1
  putStrLn "  Resultado: No acepta (necesita 'ab' completo)"
  putStrLn ""

  -- Prueba 2: "b" (no empieza bien)
  putStrLn "  Entrada: \"b\""
  let pares2 = runMDD mdd "b"
  let clasificados2 = map (\(_, lx) -> (impMu lx, lx)) pares2
  putStrLn "  Tokens:"
  mapM_ imprimirPar clasificados2
  putStrLn "  Resultado: Error lexico (no acepta 'b' solo)"
  putStrLn ""

  -- Prueba 3: "ab" (correcto)
  putStrLn "  Entrada: \"ab\""
  let pares3 = runMDD mdd "ab"
  let clasificados3 = map (\(_, lx) -> (impMu lx, lx)) pares3
  putStrLn "  Tokens:"
  mapM_ imprimirPar clasificados3
  putStrLn "  Resultado: ACEPTA - 'ab' es reconocido como Id"
  putStrLn ""

  -- Prueba 4: "abb" (maximal munch)
  putStrLn "  Entrada: \"abb\""
  let pares4 = runMDD mdd "abb"
  let clasificados4 = map (\(_, lx) -> (impMu lx, lx)) pares4
  putStrLn "  Tokens:"
  mapM_ imprimirPar clasificados4
  putStrLn "  Resultado: Maximal munch - reconoce 'ab', sobra 'b' (error)"
  putStrLn ""

  -- Prueba 5: "abc"
  putStrLn "  Entrada: \"abc\""
  let pares5 = runMDD mdd "abc"
  let clasificados5 = map (\(_, lx) -> (impMu lx, lx)) pares5
  putStrLn "  Tokens:"
  mapM_ imprimirPar clasificados5
  putStrLn "  Resultado: Maximal munch - reconoce 'ab', sobra 'c' (error)"
  putStrLn ""

  -- ============================================
  -- PARTE 2: Lexer IMP (rápido) + Tabla bonita
  -- ============================================
  putStrLn "============================================"
  putStrLn "PARTE 2: LEXER PARA IMP"
  putStrLn "============================================"
  putStrLn ""

  let progIMP = "if x <= 10 then skip else x := x + 1"

  putStrLn "Programa (IMP):"
  putStrLn progIMP
  putStrLn ""
  putStrLn "Tokens (IMP):"
  case scanIMP progIMP of
    Left err    -> putStrLn ("LEX ERROR: " ++ err)
    Right toks  -> renderTokens toks