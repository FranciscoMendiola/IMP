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

digito :: ExprReg
digito = foldr1 (##) (map term "0123456789")

minuscula :: ExprReg
minuscula = foldr1 (##) (map term "abcdefghijklmnopqrstuvwxyz")

mayuscula :: ExprReg
mayuscula = foldr1 (##) (map term "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

letra :: ExprReg
letra = minuscula ## mayuscula

enteroLitER :: ExprReg
enteroLitER = digito $$ kleene digito

identificadorER :: ExprReg
identificadorER = letra $$ kleene (letra ## digito)

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

opMas, opMenos, opPor, opAsignar, opIgual, opMenorIgual :: ExprReg
delimPuntoYComa, delimParIzq, delimParDer :: ExprReg

opMas         = term '+'
opMenos       = term '-'
opPor         = term '*'
opAsignar     = term ':' $$ term '='
opIgual       = term '='
opMenorIgual  = term '<' $$ term '='
delimPuntoYComa = term ';'
delimParIzq     = term '('
delimParDer     = term ')'

-- Espacios: ( |\t|\n|\r)+
espaciosER :: ExprReg
espaciosER = (term ' ' ## term '\t' ## term '\n' ## term '\r')
    $$ kleene (term ' ' ## term '\t' ## term '\n' ## term '\r')

-- Comentario (DEFINIDO pero NO usado en impER para evitar explosión del AFD)
-- # seguido de cualquier char != '\n'
noNuevaLinea :: ExprReg
noNuevaLinea =
  foldr1 (##) (map term ("0123456789" ++
                         "abcdefghijklmnopqrstuvwxyz" ++
                         "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
                         " \t\r" ++
                         "!\"$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))

comentarioER :: ExprReg
comentarioER = term '#' $$ kleene noNuevaLinea

-- ============================================
-- ER completa de IMP (SIN comentarioER)
-- ============================================

impER :: ExprReg
impER = kwIf ## kwThen ## kwElse ## kwWhile ## kwDo ## kwSkip
      ## kwTrue ## kwFalse ## kwNot ## kwAnd
      ## opAsignar ## opMenorIgual      -- 2-char primero
      ## opMas ## opMenos ## opPor ## opIgual
      ## delimPuntoYComa ## delimParIzq ## delimParDer
      ## identificadorER
      ## enteroLitER
      ## espaciosER
      -- ## comentarioER  

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

  _ | all esDigito lexema && not (null lexema) -> Just IntLit
    | not (null lexema) && esLetra (head lexema) && all esAlfaNum (tail lexema) -> Just Id
    | all isSpace lexema && not (null lexema) -> Just WS
    | not (null lexema) && head lexema == '#' -> Just Comment
    | otherwise -> Nothing
  where
    esDigito c  = c >= '0' && c <= '9'
    esLetra c   = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    esAlfaNum c = esLetra c || esDigito c

-- ============================================
-- Construcción del MDD y lexer de IMP
-- ============================================

construirImpMDD :: MDD
construirImpMDD =
  let afnep  = exprRegToAFNEp impER
      afn    = afnEpToAFN afnep
      afd    = afnToAfd afn
      afdMin = afdToAfdMin afd
      mu     = Map.fromList [ (q, Id) | q <- finalesM afdMin ] 
  in afdMinToMDD afdMin mu

-- API final: programa -> tokens
lexImpTokens :: String -> Either String [Token]
lexImpTokens =
  let mdd = construirImpMDD
  in buildLexer mdd impMu