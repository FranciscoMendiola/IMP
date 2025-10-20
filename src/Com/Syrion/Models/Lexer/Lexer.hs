module Com.Syrion.Models.Lexer.Lexer
  ( Token(..)
  , lexer
  , buildLexer
  ) where

import Com.Syrion.Models.Automata.MDD (MDD, runMDD)
import Com.Syrion.Models.Lexer.Token (TokenKind(..))
import qualified Data.Map.Strict as Map

-- ============================================
-- Tipo Token: representa un token con posición
-- ============================================

data Token = Token
  { tokenKind :: TokenKind
  , lexeme    :: String
  , position  :: Int  -- Posición en la entrada (opcional, para errores)
  } deriving (Eq, Show)

-- ============================================
-- Función lexer principal
-- ============================================

-- | Convierte MDD + función de clasificación en un lexer completo
-- La función mu clasifica lexemas en sus tipos de token
-- CAMBIO: Devuelve Either String [Token] para manejar errores léxicos
lexer :: MDD -> (String -> Maybe TokenKind) -> String -> Either String [Token]
lexer mdd mu input =
  let rawTokens = runMDD mdd input
  in classifyAndFilter rawTokens mu 0

-- Clasifica tokens usando mu, filtra ignorables y maneja errores
classifyAndFilter :: [(Maybe TokenKind, String)]
                  -> (String -> Maybe TokenKind)
                  -> Int
                  -> Either String [Token]
classifyAndFilter [] _ _ = Right [] -- Caso base: éxito
classifyAndFilter ((_, lex):rest) mu pos =
  case mu lex of
    Just WS ->
      -- Ignorar espacios en blanco
      classifyAndFilter rest mu (pos + length lex)

    Just Comment ->
      -- Ignorar comentarios
      classifyAndFilter rest mu (pos + length lex)

    Just kind ->
      -- Token válido
      let currentToken = Token kind lex pos
      -- Usar (<$>) (fmap) para aplicar (:) a un resultado 'Either'
      in (currentToken :) <$> classifyAndFilter rest mu (pos + length lex)

    Nothing ->
      -- Error léxico: devolver un Left con el mensaje
      Left $ "Error lexico en la posicion " ++ show pos ++ ": secuencia no reconocida '" ++ lex ++ "'"

-- ============================================
-- Constructor de lexer
-- ============================================

-- | Construye un lexer a partir de un MDD y función de clasificación
-- CAMBIO: Devuelve una función que produce un Either String [Token]
buildLexer :: MDD -> (String -> Maybe TokenKind) -> (String -> Either String [Token])
buildLexer mdd mu = lexer mdd mu