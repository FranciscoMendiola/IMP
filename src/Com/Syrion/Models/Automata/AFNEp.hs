-- src/Com/Syrion/Models/Automata/AFNEp.hs
module Com.Syrion.Models.Automata.AFNEp

  ( Estado, Simbolo, TransEps
  , AFNEp(..)
  , exprRegToAFNEp      -- ER -> AFN-ε (Thompson)
  , prettyAFNEp
  ) where

import Data.List (nub)
import Com.Syrion.Models.Regex.ExprReg
  ( ExprReg(..) )

type Estado  = String
type Simbolo = Char

-- Transiciones AFNEp (origen, etiqueta, destinos) donde etiqueta = Nothing es transición ε
type TransEps = (Estado, Maybe Simbolo, [Estado])

-- Único estado final (Thompson)
data AFNEp = AFNEp
  { estados      :: [Estado]
  , alfabeto     :: [Simbolo]
  , transiciones :: [TransEps]
  , inicial      :: Estado
  , final        :: Estado
  } deriving (Show)

-- Construcción de Thompson: ExprReg -> AFNEp
exprRegToAFNEp :: ExprReg -> AFNEp
exprRegToAFNEp er = fst (go er 0)
  where
    -- go :: ExprReg -> nextId -> (AFNEp, nextId')
    go :: ExprReg -> Int -> (AFNEp, Int)
    go Epsilon n =
      let q0 = q n; q1 = q (n+1)
      in (mk [q0,q1] [] [(q0, Nothing, [q1])] q0 q1, n+2)

    go (Term c) n =
      let q0 = q n; q1 = q (n+1)
      in (mk [q0,q1] [c] [(q0, Just c, [q1])] q0 q1, n+2)

    -- Or (disyunción): nuevo ini con ε a ini1/ini2; finales de submáquinas ε -> final nuevo
    go (Or r s) n =
      let (m1, n1) = go r n
          (m2, n2) = go s n1
          q0 = q n2; qf = q (n2+1)
          es = nub (estados m1 ++ estados m2 ++ [q0,qf])
          sig = nub (alfabeto m1 ++ alfabeto m2)
          ts = transiciones m1 ++ transiciones m2
             ++ [ (q0, Nothing, [inicial m1, inicial m2])
                , (final m1, Nothing, [qf])
                , (final m2, Nothing, [qf])
                ]
      in (mk es sig ts q0 qf, n2+2)

    -- And (concatenación): final1 --ε--> ini2
    go (And r s) n =
      let (m1, n1) = go r n
          (m2, n2) = go s n1
          es = nub (estados m1 ++ estados m2)
          sig = nub (alfabeto m1 ++ alfabeto m2)
          ts = transiciones m1 ++ transiciones m2
             ++ [ (final m1, Nothing, [inicial m2]) ]
      in (mk es sig ts (inicial m1) (final m2), n2)

    -- Kleene: nuevo ini q0 con ε-> ini1 y ε-> qf; final1 ε-> ini1 y ε-> qf
    go (Kleene r) n =
      let (m1, n1) = go r n
          q0 = q n1; qf = q (n1+1)
          es = nub (estados m1 ++ [q0,qf])
          sig = alfabeto m1
          ts = transiciones m1
             ++ [ (q0,       Nothing, [inicial m1, qf])
                , (final m1, Nothing, [inicial m1, qf])
                ]
      in (mk es sig ts q0 qf, n1+2)

    q i = "q" ++ show i
    mk = AFNEp

-- Pretty-print: Representación como cadena (String).
prettyAFNEp :: AFNEp -> String
prettyAFNEp m =
  unlines $
    [ "Estados:   " ++ show (estados m)
    , "Alfabeto:  " ++ show (alfabeto m)
    , "Inicial:   " ++ inicial m
    , "Final:     " ++ final m
    , "Transiciones:"
    ] ++ map p (transiciones m)
  where
    p (q, Nothing , ds) = "  " ++ q ++ " --ε--> " ++ show ds
    p (q, Just c  , ds) = "  " ++ q ++ " --"++[c]++"--> " ++ show ds
