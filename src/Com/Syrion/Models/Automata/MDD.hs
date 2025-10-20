-- src/Com/Syrion/Models/Automata/MDD.hs

module Com.Syrion.Models.Automata.MDD
  ( MDD(..)
  , afdMinToMDD
  , runMDD
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Com.Syrion.Models.Automata.AFDMin (AFDmin(..))
import Com.Syrion.Models.Lexer.Token (TokenKind(..))

-- MDD formal
data MDD = MDD
  { estadosMDD :: [Int]
  , alfabetoMDD :: [Char]
  , deltaMDD :: Map (Int, Char) Int
  , inicialMDD :: Int
  , finalesMDD :: [Int]
  , muMDD :: Map Int TokenKind
  } deriving (Show)

-- AFDmin -> MDD
afdMinToMDD :: AFDmin -> Map Int TokenKind -> MDD
afdMinToMDD afdmin mu =
  let sig = alfabetoM afdmin
      qs = estadosM afdmin
      q0 = inicialM afdmin
      fs = finalesM afdmin
      delta = Map.fromList [ ((p,a), q) | (p,a,q) <- transicionesM afdmin ]
  in MDD qs sig delta q0 fs mu

-- Reconocimiento con prefijo máximo y retroceso
-- VERSIÓN CORREGIDA
runMDD :: MDD -> String -> [(Maybe TokenKind, String)]
runMDD mdd input = go (inicialMDD mdd) "" input Nothing []
  where
    go _ _ [] Nothing acc = reverse acc  -- Fin sin token pendiente
    go _ _ [] (Just (tok, lex)) acc = reverse ((tok, lex) : acc)  -- Emitir último token
    
    go q lexema (x:xs) ultimo acc =
      case Map.lookup (q, x) (deltaMDD mdd) of
        Just q' ->
          -- Hay transición, continuar
          let lex' = lexema ++ [x]
              -- Actualizar último match si q' es final
              ultimo' = if q' `elem` finalesMDD mdd
                        then Just (Map.lookup q' (muMDD mdd), lex')
                        else ultimo
          in go q' lex' xs ultimo' acc
        
        Nothing ->
          -- No hay transición, retroceder al último match
          case ultimo of
            Just (Just tok, pref) ->
              -- Emitir el token y reiniciar
              let resto = drop (length pref) (lexema ++ x:xs)
              in go (inicialMDD mdd) "" resto Nothing ((Just tok, pref) : acc)
            
            Just (Nothing, pref) ->
              -- Estado final pero sin TokenKind (error interno)
              let resto = drop (length pref) (lexema ++ x:xs)
              in go (inicialMDD mdd) "" resto Nothing ((Nothing, pref) : acc)
            
            Nothing ->
              -- Sin match previo: error léxico en el carácter actual
              -- Consumir solo el lexema acumulado como error
              if null lexema
                then go (inicialMDD mdd) "" xs Nothing ((Nothing, [x]) : acc)
                else go (inicialMDD mdd) "" (x:xs) Nothing ((Nothing, lexema) : acc)