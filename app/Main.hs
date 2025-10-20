-- app/Main.hs
module Main where

import System.IO (hSetEncoding, stdout, utf8)

import Com.Syrion.Models.Regex.ExprReg
  ( ExprReg(..),
    kleene,
    term,
    (##),
    ($$)
  )

import Com.Syrion.Models.Automata.AFNEp
  ( exprRegToAFNEp,
    prettyAFNEp
  )

import Com.Syrion.Models.Automata.AFN
  ( afnEpToAFN,
    prettyAFN
  )

import Com.Syrion.Models.Automata.AFD
  ( afnToAfd,
    prettyAFD
  )

import Com.Syrion.Models.Automata.AFDMin
  ( afdToAfdMin,
    prettyAFDmin,
    finalesM
  )

import Com.Syrion.Models.Automata.MDD (MDD, afdMinToMDD, runMDD)
import Com.Syrion.Models.Lexer.Token (TokenKind(..))
import qualified Data.Map.Strict as Map

-- Importar las funciones de IMP
import Com.Syrion.Models.Lenguajes.ImpSpec (impER, impMu)

main :: IO ()
main = do
  -- Configurar UTF-8 para Windows
  hSetEncoding stdout utf8
  
  putStrLn "=========================================="
  putStrLn "  ANALIZADOR LEXICO - PIPELINE COMPLETO"
  putStrLn "  ER -> AFNeps -> AFN -> AFD -> AFDmin -> MDD -> LEXER"
  putStrLn "=========================================="
  putStrLn ""
  
  -- ============================================
  -- PARTE 1: Demostración con ER simple (ab)
  -- ============================================
  putStrLn "============================================"
  putStrLn "PARTE 1: Pipeline completo con ER simple"
  putStrLn "============================================"
  putStrLn ""
  putStrLn "--- ER: a concatenado con b (ab) ---"
  let er = (term 'a') $$ (term 'b')
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
  let mdd = afdMinToMDD afdMin mu
  
  putStrLn "== Paso 5: MDD (Maquina Discriminadora Determinista) =="
  putStrLn "MDD construido exitosamente"
  putStrLn ""

  -- Paso 6: MDD -> LEXER (aplicando funcion mu)
  putStrLn "== Paso 6: LEXER (MDD + funcion mu) =="
  putStrLn "Tokenizando cadenas de prueba:"
  putStrLn ""
  
  -- Prueba 1: "a" (incompleto)
  putStrLn "  Entrada: \"a\""
  let tokens1 = runMDD mdd "a"
  let classified1 = map (\(_, lex) -> (impMu lex, lex)) tokens1
  putStr "  Tokens:  "
  print classified1
  putStrLn "  Resultado: No acepta (necesita 'ab' completo)"
  putStrLn ""
  
  -- Prueba 2: "b" (no empieza bien)
  putStrLn "  Entrada: \"b\""
  let tokens2 = runMDD mdd "b"
  let classified2 = map (\(_, lex) -> (impMu lex, lex)) tokens2
  putStr "  Tokens:  "
  print classified2
  putStrLn "  Resultado: Error lexico (no acepta 'b' solo)"
  putStrLn ""
  
  -- Prueba 3: "ab" (correcto)
  putStrLn "  Entrada: \"ab\""
  let tokens3 = runMDD mdd "ab"
  let classified3 = map (\(_, lex) -> (impMu lex, lex)) tokens3
  putStr "  Tokens:  "
  print classified3
  putStrLn "  Resultado: ACEPTA - 'ab' es reconocido como Id"
  putStrLn ""
  
  -- Prueba 4: "abb" (maximal munch)
  putStrLn "  Entrada: \"abb\""
  let tokens4 = runMDD mdd "abb"
  let classified4 = map (\(_, lex) -> (impMu lex, lex)) tokens4
  putStr "  Tokens:  "
  print classified4
  putStrLn "  Resultado: Maximal munch - reconoce 'ab', sobra 'b' (error)"
  putStrLn ""
  
  -- Prueba 5: "abc"
  putStrLn "  Entrada: \"abc\""
  let tokens5 = runMDD mdd "abc"
  let classified5 = map (\(_, lex) -> (impMu lex, lex)) tokens5
  putStr "  Tokens:  "
  print classified5
  putStrLn "  Resultado: Maximal munch - reconoce 'ab', sobra 'c' (error)"
  putStrLn ""
  
  -- ============================================
  -- PARTE 2: TU PARTE - Lexer para IMP
  -- ============================================
  putStrLn ""
  putStrLn "============================================"
  putStrLn "PARTE 2: LEXER PARA IMP (version simplificada)"
  putStrLn "============================================"
  putStrLn ""
  
  -- ER simplificada para IMP: if | x
  putStrLn ">>> Construyendo MDD para IMP (if | x)..."
  let simpleER = (term 'i' $$ term 'f') ## (term 'x')
  
  -- Pipeline completo
  let simpleNfae = exprRegToAFNEp simpleER
  let simpleNfa = afnEpToAFN simpleNfae
  let simpleAfd = afnToAfd simpleNfa
  let simpleAfdMin = afdToAfdMin simpleAfd
  let simpleMu = Map.fromList [ (q, Id) | q <- finalesM simpleAfdMin ]
  let simpleMDD = afdMinToMDD simpleAfdMin simpleMu
  
  putStrLn "OK MDD de IMP construido"
  putStrLn ""
  
  -- LEXER: MDD + impMu
  putStrLn ">>> LEXER: Aplicando funcion mu (impMu) para clasificar..."
  putStrLn ""
  
  -- Test 1: "if" (palabra reservada)
  putStrLn "--- Test 1: Palabra reservada ---"
  putStrLn "  Entrada: \"if\""
  let test1 = runMDD simpleMDD "if"
  let class1 = map (\(_, lex) -> (impMu lex, lex)) test1
  putStr "  Tokens:  "
  mapM_ printToken class1
  putStrLn "  Clasificacion: KIf (palabra reservada, NO identificador)"
  putStrLn ""
  
  -- Test 2: "x" (identificador)
  putStrLn "--- Test 2: Identificador ---"
  putStrLn "  Entrada: \"x\""
  let test2 = runMDD simpleMDD "x"
  let class2 = map (\(_, lex) -> (impMu lex, lex)) test2
  putStr "  Tokens:  "
  mapM_ printToken class2
  putStrLn "  Clasificacion: Id (identificador)"
  putStrLn ""
  
  -- Test 3: "iff" (identificador, no palabra reservada)
  putStrLn "--- Test 3: Desambiguacion ---"
  putStrLn "  Entrada: \"iff\""
  let test3 = runMDD simpleMDD "iff"
  let class3 = map (\(_, lex) -> (impMu lex, lex)) test3
  putStr "  Tokens:  "
  mapM_ printToken class3
  putStrLn "  Clasificacion: Maximal munch reconoce 'if', sobra 'f'"
  putStrLn ""

-- ============================================
-- Funciones auxiliares
-- ============================================

printToken :: (Maybe TokenKind, String) -> IO ()
printToken (Just tok, lex) = putStrLn $ "    " ++ padRight 12 (show tok) ++ " => " ++ show lex
printToken (Nothing, lex)  = putStrLn $ "    ERROR        => " ++ show lex

padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '