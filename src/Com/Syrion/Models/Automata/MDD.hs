-- src/Com/Syrion/Models/Automata/MDD.hs
module Com.Syrion.Models.Automata.MDD
  ( MDD(..)
  , afdMinToMDD      -- AFDmin -> MDD
  , afdToMDD         -- AFD    -> MDD (sin minimizar)
  , runMDD           -- ejecución completa (si la quieres)
  , longestPrefixMDD -- 👈 prefijo máximo sin consumir toda la entrada
  , prettyMDD
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Com.Syrion.Models.Automata.AFDMin (AFDmin(..))
import Com.Syrion.Models.Automata.AFD    (AFD(..))
import Com.Syrion.Models.Lexer.Token     (TokenKind(..))

-- =======================
-- Definición de la MDD
-- =======================
data MDD = MDD
  { estadosMDD  :: [Int]
  , alfabetoMDD :: [Char]
  , deltaMDD    :: Map (Int, Char) Int
  , inicialMDD  :: Int
  , finalesMDD  :: [Int]
  , muMDD       :: Map Int TokenKind
  } deriving (Eq, Show)

-- =======================
-- Construcciones
-- =======================
afdMinToMDD :: AFDmin -> Map Int TokenKind -> MDD
afdMinToMDD afdmin mu =
  let sig   = alfabetoM afdmin
      qs    = estadosM afdmin
      q0    = inicialM afdmin
      fs    = finalesM afdmin
      delta = Map.fromList [ ((p,a), q) | (p,a,q) <- transicionesM afdmin ]
  in MDD qs sig delta q0 fs mu

afdToMDD :: AFD -> Map Int TokenKind -> MDD
afdToMDD afd mu =
  let statesD  = estadosD afd
      sigma    = alfabetoD afd
      transD   = transicionesD afd
      startD   = inicialD afd
      finalsD' = finalesD afd
      numbered = zip [0..] statesD
      findId s = case [ i | (i,s') <- numbered, s' == s ] of
                   (i:_) -> i
                   _     -> error "afdToMDD: estado determinista no encontrado"
      qs     = map fst numbered
      q0     = findId startD
      fs     = map findId finalsD'
      deltaM = Map.fromList [ ((findId p, a), findId q) | (p,a,q) <- transD ]
  in MDD qs sigma deltaM q0 fs mu

-- =======================
-- Runner (completo)
-- =======================
runMDD :: MDD -> String -> [(Maybe TokenKind, String)]
runMDD mdd input = go (inicialMDD mdd) "" input Nothing []
  where
    go _ _ [] Nothing acc = reverse acc
    go _ _ [] (Just (tok, lexm)) acc = reverse ((tok, lexm) : acc)
    go q lexema (x:xs) ultimo acc =
      case Map.lookup (q, x) (deltaMDD mdd) of
        Just q' ->
          let lex'    = lexema ++ [x]
              ultimo' = if q' `elem` finalesMDD mdd
                        then Just (Map.lookup q' (muMDD mdd), lex')
                        else ultimo
          in go q' lex' xs ultimo' acc
        Nothing ->
          case ultimo of
            Just (Just tok, pref) ->
              let resto = drop (length pref) (lexema ++ x:xs)
              in go (inicialMDD mdd) "" resto Nothing ((Just tok, pref) : acc)
            Just (Nothing, pref) ->
              let resto = drop (length pref) (lexema ++ x:xs)
              in go (inicialMDD mdd) "" resto Nothing ((Nothing, pref) : acc)
            Nothing ->
              if null lexema
                then go (inicialMDD mdd) "" xs Nothing ((Nothing, [x]) : acc)
                else go (inicialMDD mdd) "" (x:xs) Nothing ((Nothing, lexema) : acc)

-- =======================
-- Prefijo máximo (local)
-- =======================
-- Devuelve el lexema más largo aceptado desde el inicio del string dado.
-- No consume más allá (sirve para combinar varios MDDs por prioridad).
longestPrefixMDD :: MDD -> String -> String
longestPrefixMDD mdd s = go (inicialMDD mdd) "" s Nothing
  where
    go _ acc [] lastOk =
      case lastOk of
        Just best -> best
        Nothing   -> ""
    go q acc (x:xs) lastOk =
      case Map.lookup (q, x) (deltaMDD mdd) of
        Just q' ->
          let acc'   = acc ++ [x]
              lastOk' = if q' `elem` finalesMDD mdd
                        then Just acc'
                        else lastOk
          in go q' acc' xs lastOk'
        Nothing ->
          case lastOk of
            Just best -> best
            Nothing   -> ""

-- =======================
-- Debug
-- =======================
prettyMDD :: MDD -> String
prettyMDD m =
  let hdr =
        [ "MDD"
        , "  #estados = " ++ show (length (estadosMDD m))
        , "  alfabeto = " ++ show (alfabetoMDD m)
        , "  inicial  = q" ++ show (inicialMDD m)
        , "  finales  = " ++ show (map (\q -> "q"++show q) (finalesMDD m))
        , "  #delta   = " ++ show (Map.size (deltaMDD m))
        , "  #mu      = " ++ show (Map.size (muMDD m))
        , "  transiciones:"
        ]
      ts =
        [ "    q" ++ show p ++ " --" ++ [a] ++ "--> q" ++ show q
        | ((p,a), q) <- Map.toList (deltaMDD m)
        ]
  in unlines (hdr ++ ts)
