module Com.Syrion.Models.Lexer.Lexer
  ( Token(..)
  , lexer
  , buildLexer
  ) where

import Com.Syrion.Models.Automata.MDD (MDD, runMDD)
import Com.Syrion.Models.Lexer.Token (TokenKind(..))
import qualified Data.Map.Strict as Map

-- ============================================
-- Tipo Token
-- ============================================

data Token = Token
  { tokenKind :: TokenKind
  , lexeme    :: String
  , position  :: Int
  } deriving (Eq, Show)

-- ============================================
-- Núcleo del lexer (sin pre-scan de comentarios)
-- ============================================

lexer :: MDD -> (String -> Maybe TokenKind) -> String -> Either String [Token]
lexer mdd mu input =
  let rawTokens = runMDD mdd input
  in classifyAndFilter rawTokens mu 0

classifyAndFilter :: [(Maybe TokenKind, String)]
                  -> (String -> Maybe TokenKind)
                  -> Int
                  -> Either String [Token]
classifyAndFilter [] _ _ = Right []
classifyAndFilter ((_, lex):rest) mu pos =
  case mu lex of
    Just WS      -> classifyAndFilter rest mu (pos + length lex)
    Just Comment -> classifyAndFilter rest mu (pos + length lex)
    Just kind    ->
      let tok = Token kind lex pos
      in (tok :) <$> classifyAndFilter rest mu (pos + length lex)
    Nothing ->
      Left $ "Error lexico en la posicion " ++ show pos ++ ": secuencia no reconocida '" ++ lex ++ "'"

-- ============================================
-- Pre-scan de comentarios: versión ligera
-- ============================================
-- Estrategia:
--  - Partimos la entrada en segmentos: [noComentario, comentario, noComentario, ...]
--  - Los segmentos de comentario (#...hasta \n) se IGNORAN (o podrías convertirlos a Token).
--  - Los segmentos noComentario se envían a runMDD + classifyAndFilter.
--  - Mantenemos la posición acumulada correcta.

buildLexer :: MDD -> (String -> Maybe TokenKind) -> (String -> Either String [Token])
buildLexer mdd mu = lexWithCommentPrescan
  where
    lexWithCommentPrescan :: String -> Either String [Token]
    lexWithCommentPrescan = go 0
      where
        go :: Int -> String -> Either String [Token]
        go _   [] = Right []
        go pos ('#':xs) =
          -- Consumir hasta \n (sin consumir el '\n'), ignorándolo
          let (commentBody, rest) = span (/= '\n') xs
              consumed = 1 + length commentBody  -- '#'+cuerpo
              pos' = pos + consumed
          in go pos' rest  -- ignoramos comentario
        go pos s =
          -- Consumir hasta el próximo '#' o fin: tramo "normal"
          let (chunk, rest) = break (== '#') s
              pos' = pos + length chunk
              raw  = runMDD mdd chunk
          in case classifyAndFilter raw mu pos of
               Left e     -> Left e
               Right toks -> (toks ++) <$> go pos' rest
