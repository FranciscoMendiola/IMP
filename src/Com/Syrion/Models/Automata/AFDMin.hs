-- src/Com/Syrion/Models/Automata/AFDMin.hs
module Com.Syrion.Models.Automata.AFDMin
  ( AFDmin(..)
  , afdToAfdMin
  , prettyAFDmin
  ) where

import Data.List (nub, foldl')
import qualified Data.Map.Strict as M

import Com.Syrion.Models.Automata.AFD (AFD(..), EstadoD)
import Com.Syrion.Models.Automata.AFN (Simbolo)

data AFDmin = AFDmin
  { estadosM      :: [Int]
  , alfabetoM     :: [Simbolo]
  , transicionesM :: [(Int, Simbolo, Int)]
  , inicialM      :: Int
  , finalesM      :: [Int]
  } deriving (Eq, Show)

ordPair :: (Ord a) => (a,a) -> (a,a)
ordPair (x,y) = if x <= y then (x,y) else (y,x)

-- Union-Find
ufFind :: (Ord a) => a -> M.Map a a -> (a, M.Map a a)
ufFind x parent =
  case M.lookup x parent of
    Nothing   -> (x, M.insert x x parent)
    Just p
      | p == x  -> (x, parent)
      | otherwise ->
          let (r, parent') = ufFind p parent
          in (r, M.insert x r parent')

ufUnion :: (Ord a) => a -> a -> M.Map a a -> M.Map a a
ufUnion a b parent =
  let (ra, p1) = ufFind a parent
      (rb, p2) = ufFind b p1
  in if ra == rb then p2 else M.insert rb ra p2

agrupar :: (Ord a) => [a] -> [(a,a)] -> [[a]]
agrupar xs pares =
  let parent0 = M.fromList [ (x, x) | x <- xs ]
      parentU = foldl' (\par (u,v) -> ufUnion u v par) parent0 pares
      (_, rootsList) =
        foldl' (\(par, acc) x ->
                  let (r, par') = ufFind x par
                  in (par', (x, r):acc)
               ) (parentU, []) xs
      buckets = foldl' (\m (x,r) -> M.insertWith (++) r [x] m) M.empty rootsList
  in map reverse (M.elems buckets)

afdToAfdMin :: AFD -> AFDmin
afdToAfdMin afd =
  let
      qs  = estadosD afd
      fs  = finalesD afd
      sig = alfabetoD afd
      ts  = transicionesD afd
      q0  = inicialD afd

      -- Pares (p<q)
      pares = [ (p,q) | p <- qs, q <- qs, p < q ]

      -- Base: final vs no final
      baseMarcados =
        [ ordPair (p,q)
        | (p,q) <- pares
        , (p `elem` fs) /= (q `elem` fs)
        ]

      deltaM p a =
        case [ q2 | (p1,a1,q2) <- ts, p1==p, a1==a ] of
          (q2:_) -> Just q2
          _      -> Nothing

      marcaPaso marcados =
        [ (p,q)
        | (p,q) <- pares
        , ordPair (p,q) `notElem` marcados
        , any (\a ->
            case (deltaM p a, deltaM q a) of
              (Just dp, Just dq) -> ordPair (dp, dq) `elem` marcados
              (Nothing, Just _)  -> True
              (Just _, Nothing)  -> True
              (Nothing, Nothing) -> False
          ) sig
        ]

      marcar marcados =
        let nuevos = marcaPaso marcados
            marcados' = marcados ++ map ordPair nuevos
        in if null nuevos then marcados else marcar marcados'

      marcados = marcar baseMarcados

      -- Pares NO marcados → equivalentes
      eqPairs = [ ordPair (p,q) | (p,q) <- pares, ordPair (p,q) `notElem` marcados ]

      -- Clases de equivalencia
      clases = agrupar qs eqPairs

      -- Renombrado 0..k-1
      clasesNum = zip [0..] clases
      nombre q  = head [ n | (n, c) <- clasesNum, q `elem` c ]

      -- Reconstruir δ' sólo con transiciones existentes (sin sumidero)
      tsM = nub [ (nombre p, a, nombre q) | (p,a,q) <- ts ]
      fM  = nub [ nombre q | q <- fs ]
      q0M = nombre q0
      estados = [0 .. length clases - 1]

  in AFDmin estados sig tsM q0M fM

prettyAFDmin :: AFDmin -> String
prettyAFDmin afd =
  let tsV = transicionesM afd
      qsV = nub $ [ inicialM afd ] ++ finalesM afd
                 ++ [ p | (p,_,_) <- tsV ] ++ [ q | (_,_,q) <- tsV ]
  in unlines $
    [ "Estados AFDmin: " ++ show qsV
    , "Alfabeto:       " ++ show (alfabetoM afd)
    , "Inicial:        q" ++ show (inicialM afd)
    , "Finales:        " ++ show (map (\q -> "q"++show q) (finalesM afd))
    , "Transiciones:"
    ] ++ [ "  q"++show p ++ " --"++[a]++"--> q"++show q
         | (p,a,q) <- tsV ]
