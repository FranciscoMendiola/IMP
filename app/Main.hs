module Main where
  
import Com.Syrion.Models.Regex.ExprReg

  ( ExprReg(..)
  , term
  , kleene
  , ($$)
  , (##)
  )

main :: IO ()
main = do
  -- expresión: (a+b)*c
  let e1 = kleene (term 'a' ## term 'b') $$ term 'c'
  putStrLn $ "Expresión 1: " ++ show e1

  -- expresión: (a+b)(c+d)
  let e2 = (term 'a' ## term 'b') $$ (term 'c' ## term 'd')
  putStrLn $ "Expresión 2: " ++ show e2

  -- expresión: a(b+c)*
  let e3 = term 'a' $$ kleene (term 'b' ## term 'c')
  putStrLn $ "Expresión 3: " ++ show e3
