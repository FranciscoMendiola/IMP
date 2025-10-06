module Com.Syrion.Models.Lexer.Token
  ( TokenKind(..) ) where

data TokenKind
  -- Palabras reservadas
  = KIf | KThen | KElse | KWhile | KDo | KSkip
  | KTrue | KFalse | KNot | KAnd
  -- Identificadores y literales
  | Id | IntLit
  -- Operadores / delimitadores
  | Plus | Minus | Times | Assign | Eq | Leq | Semi | LPar | RPar
  -- Ignorables
  | WS | Comment
  deriving (Eq, Ord, Show)
