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

main :: IO ()
main = do
  -- Expresión regular de prueba: (a + b)*c
  let er = kleene (term 'a' ## term 'b') $$ term 'c'
  putStrLn $ "ER: " ++ show er

  -- ER → AFNε (Thompson)
  let nfae = exprRegToAFNEp er
  putStrLn "\n== AFN-ε (Thompson) =="
  putStrLn (prettyAFNEp nfae)

  -- AFNε → AFN (sin ε)
  let nfa = afnEpToAFN nfae
  putStrLn "\n== AFN (sin ε) =="
  putStrLn (prettyAFN nfa)

  -- AFN → AFD (determinista)
  let afd = afnToAfd nfa
  putStrLn "\n== AFD (determinista) =="
  putStrLn (prettyAFD afd)
