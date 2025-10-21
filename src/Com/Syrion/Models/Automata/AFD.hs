-- src/Com/Syrion/Models/Automata/AFD.hs
module Com.Syrion.Models.Automata.AFD
  ( EstadoD, TransDet
  , AFD(..)
  , afnToAfd
  , prettyAFD
  ) where

import Data.List (nub, sort, intercalate)

import Com.Syrion.Models.Automata.AFN
  ( AFN(..), Estado, Simbolo )

-- Estados del AFD = subconjuntos de estados del AFN
type EstadoD  = [Estado]
type TransDet = (EstadoD, Simbolo, EstadoD)

data AFD = AFD
  { estadosD      :: [EstadoD]
  , alfabetoD     :: [Simbolo]
  , transicionesD :: [TransDet]
  , inicialD      :: EstadoD
  , finalesD      :: [EstadoD]
  } deriving (Eq, Show)

-- =========================
-- AFN -> AFD (subconjuntos)
-- =========================
afnToAfd :: AFN -> AFD
afnToAfd n =
  let sig                 = alfabetoN n
      q0                  = canon [inicialN n]
      (qs, ts)            = determinize n sig [q0] [] []
      esFinal qset        = any (`elem` finalesN n) qset
      fD                  = filter esFinal qs
  in AFD { estadosD = qs
         , alfabetoD = sig
         , transicionesD = ts
         , inicialD = q0
         , finalesD = fD
         }

-- -----------------------------
-- Construcción por subconjuntos
-- -----------------------------
determinize
  :: AFN
  -> [Simbolo]
  -> [EstadoD]        -- cola de pendientes
  -> [EstadoD]        -- visitados
  -> [TransDet]       -- trans acumuladas
  -> ([EstadoD], [TransDet])
determinize _ _ [] visited ts = (visited, ts)
determinize n sig (curr:rest) visited ts
  | curr `elem` visited = determinize n sig rest visited ts
  | otherwise =
      let moves     = [ (curr, a, dest)
                      | a <- sig
                      , let dest = move n curr a
                      , not (null dest)
                      ]
          nuevos    = [ d | (_,_,d) <- moves
                         , d `notElem` visited
                         , d `notElem` rest
                         ]
      in determinize n sig (rest ++ nuevos) (curr:visited) (ts ++ moves)

-- move(T, a) = ⋃ δ(q,a) para q∈T (en el AFN sin ε)
move :: AFN -> EstadoD -> Simbolo -> EstadoD
move n qs a = canon
  [ d | (p, c, ds) <- transicionesN n
      , c == a
      , p `elem` qs
      , d <- ds
  ]

-- Normaliza subconjuntos (sin duplicados, ordenados)
canon :: [Estado] -> [Estado]
canon = sort . nub

-- =========================
-- Impresión legible del AFD
-- =========================
prettyAFD :: AFD -> String
prettyAFD d =
  unlines $
    [ "Estados AFD:   " ++ show (map showSet (estadosD d))
    , "Alfabeto:      " ++ show (alfabetoD d)
    , "Inicial:       " ++ showSet (inicialD d)
    , "Finales:       " ++ show (map showSet (finalesD d))
    , "Transiciones:"
    ] ++ map p (transicionesD d)
  where
    showSet s = "{" ++ intercalate "," s ++ "}"
    p (qs, c, ds) = "  " ++ showSet qs ++ " --" ++ [c] ++ "--> " ++ showSet ds
