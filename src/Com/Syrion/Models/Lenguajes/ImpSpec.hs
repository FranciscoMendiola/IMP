module Com.Syrion.Models.Lenguajes.ImpSpec
  ( impER
  , impMu
  , lexImpTokens
  ) where

import Com.Syrion.Models.Regex.ExprReg
import Com.Syrion.Models.Automata.AFNEp (exprRegToAFNEp)
import Com.Syrion.Models.Automata.AFN (afnEpToAFN)
import Com.Syrion.Models.Automata.AFD (afnToAfd)
import Com.Syrion.Models.Automata.AFDMin (afdToAfdMin, finalesM)
import Com.Syrion.Models.Automata.MDD (MDD, afdMinToMDD)
import Com.Syrion.Models.Lexer.Token (TokenKind(..))
import Com.Syrion.Models.Lexer.Lexer (Token(..), buildLexer)
import qualified Data.Map.Strict as Map
import Data.Char (isSpace)

-- ============================================
-- ERs base
-- ============================================

digit :: ExprReg
digit = foldr1 (##) (map term "0123456789")

lower :: ExprReg
lower = foldr1 (##) (map term "abcdefghijklmnopqrstuvwxyz")

upper :: ExprReg
upper = foldr1 (##) (map term "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

letter :: ExprReg
letter = lower ## upper

intLitER :: ExprReg
intLitER = digit $$ kleene digit

identifierER :: ExprReg
identifierER = letter $$ kleene (letter ## digit)

kwIf, kwThen, kwElse, kwWhile, kwDo, kwSkip :: ExprReg
kwTrue, kwFalse, kwNot, kwAnd :: ExprReg

kwIf    = term 'i' $$ term 'f'
kwThen  = term 't' $$ term 'h' $$ term 'e' $$ term 'n'
kwElse  = term 'e' $$ term 'l' $$ term 's' $$ term 'e'
kwWhile = term 'w' $$ term 'h' $$ term 'i' $$ term 'l' $$ term 'e'
kwDo    = term 'd' $$ term 'o'
kwSkip  = term 's' $$ term 'k' $$ term 'i' $$ term 'p'
kwTrue  = term 't' $$ term 'r' $$ term 'u' $$ term 'e'
kwFalse = term 'f' $$ term 'a' $$ term 'l' $$ term 's' $$ term 'e'
kwNot   = term 'n' $$ term 'o' $$ term 't'
kwAnd   = term 'a' $$ term 'n' $$ term 'd'

opPlus, opMinus, opTimes, opAssign, opEq, opLeq :: ExprReg
delimSemi, delimLPar, delimRPar :: ExprReg

opPlus    = term '+'
opMinus   = term '-'
opTimes   = term '*'
opAssign  = term ':' $$ term '='
opEq      = term '='
opLeq     = term '<' $$ term '='
delimSemi = term ';'
delimLPar = term '('
delimRPar = term ')'

-- Espacios: ( |\t|\n|\r)+
wsER :: ExprReg
wsER = (term ' ' ## term '\t' ## term '\n' ## term '\r')
    $$ kleene (term ' ' ## term '\t' ## term '\n' ## term '\r')

-- Comentario (DEFINIDO pero NO usado en impER para evitar explosión del AFD)
-- # seguido de cualquier char != '\n'
notNewline :: ExprReg
notNewline =
  foldr1 (##) (map term ("0123456789" ++
                         "abcdefghijklmnopqrstuvwxyz" ++
                         "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
                         " \t\r" ++
                         "!\"$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))

commentER :: ExprReg
commentER = term '#' $$ kleene notNewline

-- ============================================
-- ER completa de IMP (SIN commentER)
-- ============================================

impER :: ExprReg
impER = kwIf ## kwThen ## kwElse ## kwWhile ## kwDo ## kwSkip
      ## kwTrue ## kwFalse ## kwNot ## kwAnd
      ## opAssign ## opLeq      -- 2-char primero
      ## opPlus ## opMinus ## opTimes ## opEq
      ## delimSemi ## delimLPar ## delimRPar
      ## identifierER
      ## intLitER
      ## wsER
      -- ## commentER   -- ⛔ Desactivado para evitar explosión del AFD

-- ============================================
-- Clasificación (mu)
-- ============================================

impMu :: String -> Maybe TokenKind
impMu lexema = case lexema of
  -- Palabras reservadas
  "if"    -> Just KIf
  "then"  -> Just KThen
  "else"  -> Just KElse
  "while" -> Just KWhile
  "do"    -> Just KDo
  "skip"  -> Just KSkip
  "true"  -> Just KTrue
  "false" -> Just KFalse
  "not"   -> Just KNot
  "and"   -> Just KAnd

  -- Operadores / delimitadores
  ":=" -> Just Assign
  "<=" -> Just Leq
  "+"  -> Just Plus
  "-"  -> Just Minus
  "*"  -> Just Times
  "="  -> Just Eq
  ";"  -> Just Semi
  "("  -> Just LPar
  ")"  -> Just RPar

  _ | all isDigit lexema && not (null lexema) -> Just IntLit
    | not (null lexema) && isLetter (head lexema) && all isAlphaNum (tail lexema) -> Just Id
    | all isSpace lexema && not (null lexema) -> Just WS
    | not (null lexema) && head lexema == '#' -> Just Comment
    | otherwise -> Nothing
  where
    isDigit c    = c >= '0' && c <= '9'
    isLetter c   = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    isAlphaNum c = isLetter c || isDigit c

-- ============================================
-- Construcción del MDD y lexer de IMP
-- ============================================

buildImpMDD :: MDD
buildImpMDD =
  let afnep  = exprRegToAFNEp impER
      afn    = afnEpToAFN afnep
      afd    = afnToAfd afn
      afdMin = afdToAfdMin afd
      mu     = Map.fromList [ (q, Id) | q <- finalesM afdMin ] -- provisional
  in afdMinToMDD afdMin mu

-- API final: programa -> tokens
lexImpTokens :: String -> Either String [Token]
lexImpTokens =
  let mdd = buildImpMDD
  in buildLexer mdd impMu
