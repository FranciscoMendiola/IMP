module Com.Syrion.Models.Lexer.Lexer
  ( Token(..)
  , lexer
  , buildLexer
  ) where

import Com.Syrion.Models.Automata.MDD (MDD, runMDD)
import Com.Syrion.Models.Lexer.Token (TokenKind(..))

-- ============================================
-- Tipo Token
-- ============================================

data Token = Token
  { tokenKind :: TokenKind
  , lexeme    :: String
  , position  :: Int
  } deriving (Eq, Show)

-- ============================================
-- Núcleo del lexer 
-- ============================================

lexer :: MDD -> (String -> Maybe TokenKind) -> String -> Either String [Token]
lexer mdd mu entrada =
  let tokensEnBruto = runMDD mdd entrada
  in clasificarYFiltrar tokensEnBruto mu 0

clasificarYFiltrar :: [(Maybe TokenKind, String)]
                   -> (String -> Maybe TokenKind)
                   -> Int
                   -> Either String [Token]
clasificarYFiltrar [] _ _ = Right []
clasificarYFiltrar ((_, lex):resto) mu pos =
  case mu lex of
    Just WS      -> clasificarYFiltrar resto mu (pos + length lex)
    Just Comment -> clasificarYFiltrar resto mu (pos + length lex)
    Just tipo    ->
      let tok = Token tipo lex pos
      in (tok :) <$> clasificarYFiltrar resto mu (pos + length lex)
    Nothing ->
      Left $ "Error lexico en la posicion " ++ show pos ++ ": secuencia no reconocida '" ++ lex ++ "'"

-- ============================================
-- Pre-scan de comentarios
-- ============================================
buildLexer :: MDD -> (String -> Maybe TokenKind) -> (String -> Either String [Token])
buildLexer mdd mu = lexConPrescanComentarios
  where
    lexConPrescanComentarios :: String -> Either String [Token]
    lexConPrescanComentarios = recorrer 0
      where
        recorrer :: Int -> String -> Either String [Token]
        recorrer _   [] = Right []
        recorrer pos ('#':xs) =
          let (cuerpoComentario, resto) = span (/= '\n') xs
              consumidos = 1 + length cuerpoComentario
              pos' = pos + consumidos
          in recorrer pos' resto
        recorrer pos s =
          let (trozo, resto) = break (== '#') s
              pos' = pos + length trozo
              enBruto  = runMDD mdd trozo
          in case clasificarYFiltrar enBruto mu pos of
               Left e     -> Left e
               Right toks -> (toks ++) <$> recorrer pos' resto