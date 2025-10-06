module Main where

import Com.Syrion.Models.Automata.AFN
  ( afnEpToAFN,
    prettyAFN,
  )
import Com.Syrion.Models.Automata.AFNEp
  ( exprRegToAFNEp,
    prettyAFNEp,
  )
import Com.Syrion.Models.Regex.ExprReg
  ( ExprReg (..),
    kleene,
    term,
    (##),
    ($$),
  )

main :: IO ()
main = do
  -- (a+b)*c
  let er = kleene (term 'a' ## term 'b') $$ term 'c'
  putStrLn $ "ER: " ++ show er

  let nfae = exprRegToAFNEp er
  putStrLn "\n== AFN-ε (Thompson) =="
  putStrLn (prettyAFNEp nfae)

  let nfa = afnEpToAFN nfae
  putStrLn "\n== AFN (sin ε) =="
  putStrLn (prettyAFN nfa)
