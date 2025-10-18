-- app/Main.hs
module Main where

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
    prettyAFDmin, finalesM
  )
import qualified Data.Map.Strict as Map
import Com.Syrion.Models.Automata.MDD (afdMinToMDD, runMDD)
import Com.Syrion.Models.Lexer.Token (TokenKind(..))

main :: IO ()
main = do
  -- Expresión regular de prueba: a + b
  let er = (term 'a') $$ (term 'b')
  putStrLn $ "ER: " ++ show er

  -- ER -> AFNε (Thompson)
  let nfae = exprRegToAFNEp er
  putStrLn "\n== AFN-ε (Thompson) =="
  putStrLn (prettyAFNEp nfae)

  -- AFNε -> AFN (sin ε)
  let nfa = afnEpToAFN nfae
  putStrLn "\n== AFN (sin ε) =="
  putStrLn (prettyAFN nfa)

  -- AFN -> AFD (determinista)
  let afd = afnToAfd nfa
  putStrLn "\n== AFD (determinista) =="
  putStrLn (prettyAFD afd)

  --AFD -> AFDmin (mínimo)
  let afdMin = afdToAfdMin afd
  putStrLn "\n== AFDmin (mínimo) =="
  putStrLn (prettyAFDmin afdMin)

  let mu = Map.fromList [ (q, Id) | q <- finalesM afdMin ]
  let mdd = afdMinToMDD afdMin mu

  putStrLn "\n== MDD (desde AFDmin) =="
  print $ runMDD mdd "a"     
  print $ runMDD mdd "b"    
  print $ runMDD mdd "ab"    
  print $ runMDD mdd "abb"   
  print $ runMDD mdd "ac"     
