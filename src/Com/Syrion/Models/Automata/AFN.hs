-- src/Com/Syrion/Models/Automata/AFN.hs
module Com.Syrion.Models.Automata.AFN
  ( Estado, Simbolo, Trans
  , AFN(..)
  , afnEpToAFN           -- AFN-ε -> AFN
  , eclosure             -- ECLOSURE(q)
  , eclosureList         -- ECLOSURE(S)
  , prettyAFN
  ) where

import Data.List (nub, sort)
import qualified Data.Set as S

import Com.Syrion.Models.Automata.AFNEp
  ( Estado, Simbolo, TransEps, AFNEp(..) )

-- Transiciones AFNEp (sin ε): (origen, etiqueta, destinos)
type Trans = (Estado, Simbolo, [Estado])

-- Ahora dejamos *varios* finales (correcto en general)
data AFN = AFN
  { estadosN      :: [Estado]
  , alfabetoN     :: [Simbolo]
  , transicionesN :: [Trans]
  , inicialN      :: Estado
  , finalesN      :: [Estado]
  } deriving (Show)

-- ---------------------
-- AFN-ε -> AFN (teoría)
-- δ'(q,a) = ECLOSURE( δ( ECLOSURE(q), a ) )
-- F' = { q | ECLOSURE(q) ∩ F ≠ ∅ }
-- ---------------------
afnEpToAFN :: AFNEp -> AFN
afnEpToAFN m =
  let qs   = estados m
      sig  = alfabeto m
      ts   = transiciones m

      -- transiciones sin ε para cada estado y símbolo
      tN   = [ (q, a, sort . nub $ eclosureList m (delta ts a (eclosure m q)))
             | q <- qs, a <- sig
             ]

      -- finales: los que alcanzan al final original por ε
      fN   = [ q | q <- qs, final m `elem` eclosure m q ]
  in AFN { estadosN = qs
         , alfabetoN = sig
         , transicionesN = tN
         , inicialN = inicial m
         , finalesN = fN
         }

-- ECLOSURE(q) = estados alcanzables desde q por 0+ trans ε (incluye q)
eclosure :: AFNEp -> Estado -> [Estado]
eclosure m q0 = S.toList (go (S.singleton q0) [q0])
  where
    go vis []     = vis
    go vis (q:qs) =
      let epsDests = [ d | (p, lab, ds) <- transiciones m
                         , p == q
                         , lab == Nothing
                         , d <- ds ]
          new = filter (`S.notMember` vis) epsDests
      in go (foldr S.insert vis new) (qs ++ new)

-- ECLOSURE(S) = ⋃ ECLOSURE(q) para q∈S
eclosureList :: AFNEp -> [Estado] -> [Estado]
eclosureList m = sort . nub . concatMap (eclosure m)

-- δ(ts, a, S) = ⋃ δ(q,a) para q∈S (usando ts de AFN-ε)
delta :: [TransEps] -> Simbolo -> [Estado] -> [Estado]
delta ts a qs = nub [ d | (p, Just c, ds) <- ts
                        , c == a
                        , p `elem` qs
                        , d <- ds ]

-- Pretty-print: Representación como cadena (String).
prettyAFN :: AFN -> String
prettyAFN n =
  unlines $
    [ "Estados:   " ++ show (estadosN n)
    , "Alfabeto:  " ++ show (alfabetoN n)
    , "Inicial:   " ++ inicialN n
    , "Finales:   " ++ show (finalesN n)
    , "Transiciones:"
    ] ++ map p (transicionesN n)
  where
    p (q, c, ds) = "  " ++ q ++ " --"++[c]++"--> " ++ show ds
