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
  { estadosMDD  :: [Int]
  , alfabetoMDD :: [Char]
  , deltaMDD    :: Map (Int, Char) Int
  , inicialMDD  :: Int
  , finalesMDD  :: [Int]
  , muMDD       :: Map Int TokenKind
  } deriving (Show)

-- Detecta sumidero: no final y δ(p,a)=p para todo a
isSinkState :: [Char] -> [(Int,Char,Int)] -> [Int] -> Int -> Bool
isSinkState sigma ts finals p =
  p `notElem` finals
  && all (\a -> case [q | (p',a',q) <- ts, p'==p, a'==a] of
                  [q] -> q == p
                  _   -> False) sigma

-- AFDmin -> MDD (filtrando sumideros)
afdMinToMDD :: AFDmin -> Map Int TokenKind -> MDD
afdMinToMDD afdmin mu =
  let sig     = alfabetoM afdmin
      sinks   = [ p | p <- estadosM afdmin
                    , isSinkState sig (transicionesM afdmin) (finalesM afdmin) p ]
      qs      = [ p | p <- estadosM afdmin, p `notElem` sinks ]
      tsKeep  = [ (p,a,q) | (p,a,q) <- transicionesM afdmin
                          , p `notElem` sinks, q `notElem` sinks ]
      delta   = Map.fromList [ ((p,a),q) | (p,a,q) <- tsKeep ]
      q0      = inicialM afdmin
      fs      = [ f | f <- finalesM afdmin, f `notElem` sinks ]
  in MDD qs sig delta q0 fs mu

-- µ⋆: reconocimiento con “prefijo máximo”, retroceso y error ⊥
-- Devuelve [(Maybe TokenKind, lexema)]
runMDD :: MDD -> String -> [(Maybe TokenKind, String)]
runMDD mdd input = go (inicialMDD mdd) "" input Nothing []
  where
    go _ _ [] Nothing acc = reverse acc  -- fin sin token pendiente
    go _ _ [] (Just (tok, lex)) acc = reverse ((tok,lex):acc)

    go q lexema (x:xs) ultimo acc =
      case Map.lookup (q,x) (deltaMDD mdd) of
        Just q' ->
          let lex'       = lexema ++ [x]
              maybeTok qf = if qf `elem` finalesMDD mdd
                            then Just (Map.lookup qf (muMDD mdd), lex')
                            else Nothing
              ultimo'    = maybe ultimo Just (maybeTok q' )
          in go q' lex' xs ultimo' acc

        Nothing ->
          case ultimo of
            Just (Just tok, pref) ->
              -- emitir prefijo máximo y reiniciar en q0 sobre el resto
              let resto = drop (length pref) (lexema ++ (x:xs))
              in go (inicialMDD mdd) "" resto Nothing ((Just tok, pref):acc)
            _ ->
              -- sin aceptación previa: error léxico en x
              go (inicialMDD mdd) "" xs Nothing ((Nothing, [x]):acc)
