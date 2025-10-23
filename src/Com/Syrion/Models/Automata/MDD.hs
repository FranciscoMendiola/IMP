-- src/Com/Syrion/Models/Automata/MDD.hs
module Com.Syrion.Models.Automata.MDD
  ( MDD(..)
  , afdMinToMDD      -- AFDmin -> MDD
  , runMDD           -- ejecución completa (si la quieres)
  , longestPrefixMDD 
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

-- =======================
-- Runner (completo)
-- =======================
runMDD :: MDD -> String -> [(Maybe TokenKind, String)]
runMDD mdd entrada = recorrer (inicialMDD mdd) "" entrada Nothing []
  where
    recorrer _ _ [] Nothing acum = reverse acum
    recorrer _ _ [] (Just (tok, lexm)) acum = reverse ((tok, lexm) : acum)
    recorrer q lexema (x:xs) ultimo acum =
      case Map.lookup (q, x) (deltaMDD mdd) of
        Just q' ->
          let lex'    = lexema ++ [x]
              ultimo' = if q' `elem` finalesMDD mdd
                        then Just (Map.lookup q' (muMDD mdd), lex')
                        else ultimo
          in recorrer q' lex' xs ultimo' acum
        Nothing ->
          case ultimo of
            Just (Just tok, pref) ->
              let resto = drop (length pref) (lexema ++ x:xs)
              in recorrer (inicialMDD mdd) "" resto Nothing ((Just tok, pref) : acum)
            Just (Nothing, pref) ->
              let resto = drop (length pref) (lexema ++ x:xs)
              in recorrer (inicialMDD mdd) "" resto Nothing ((Nothing, pref) : acum)
            Nothing ->
              if null lexema
                then recorrer (inicialMDD mdd) "" xs Nothing ((Nothing, [x]) : acum)
                else recorrer (inicialMDD mdd) "" (x:xs) Nothing ((Nothing, lexema) : acum)

-- Devuelve el lexema más largo aceptado desde el inicio del string dado.
-- No consume más allá (sirve para combinar varios MDDs por prioridad).
longestPrefixMDD :: MDD -> String -> String
longestPrefixMDD mdd s = recorrer (inicialMDD mdd) "" s Nothing
  where
    recorrer _ acum [] ultimoOk =
      case ultimoOk of
        Just mejor -> mejor
        Nothing    -> ""
    recorrer q acum (x:xs) ultimoOk =
      case Map.lookup (q, x) (deltaMDD mdd) of
        Just q' ->
          let acum'    = acum ++ [x]
              ultimoOk' = if q' `elem` finalesMDD mdd
                          then Just acum'
                          else ultimoOk
          in recorrer q' acum' xs ultimoOk'
        Nothing ->
          case ultimoOk of
            Just mejor -> mejor
            Nothing    -> ""

-- =======================
-- Debug
-- =======================
prettyMDD :: MDD -> String
prettyMDD m =
  let encabezado =
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
  in unlines (encabezado ++ ts)