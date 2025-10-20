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
import Com.Syrion.Models.Automata.MDD (MDD, afdMinToMDD, runMDD)
import Com.Syrion.Models.Lexer.Token (TokenKind(..))
import Com.Syrion.Models.Lexer.Lexer (Token(..), buildLexer)
import qualified Data.Map.Strict as Map
import Data.Char (isSpace)

-- ============================================
-- Definición de ERs para cada componente léxico
-- (Se mantiene todo como lo tenías)
-- ============================================

-- Dígitos: 0|1|2|...|9
digit :: ExprReg
digit = term '0' ## term '1' ## term '2' ## term '3' ## term '4'
     ## term '5' ## term '6' ## term '7' ## term '8' ## term '9'

-- Letras minúsculas: a|b|...|z
lower :: ExprReg
lower = term 'a' ## term 'b' ## term 'c' ## term 'd' ## term 'e'
     ## term 'f' ## term 'g' ## term 'h' ## term 'i' ## term 'j'
     ## term 'k' ## term 'l' ## term 'm' ## term 'n' ## term 'o'
     ## term 'p' ## term 'q' ## term 'r' ## term 's' ## term 't'
     ## term 'u' ## term 'v' ## term 'w' ## term 'x' ## term 'y'
     ## term 'z'

-- Letras mayúsculas: A|B|...|Z
upper :: ExprReg
upper = term 'A' ## term 'B' ## term 'C' ## term 'D' ## term 'E'
     ## term 'F' ## term 'G' ## term 'H' ## term 'I' ## term 'J'
     ## term 'K' ## term 'L' ## term 'M' ## term 'N' ## term 'O'
     ## term 'P' ## term 'Q' ## term 'R' ## term 'S' ## term 'T'
     ## term 'U' ## term 'V' ## term 'W' ## term 'X' ## term 'Y'
     ## term 'Z'

-- Letra: minúscula|mayúscula
letter :: ExprReg
letter = lower ## upper

-- Enteros: digit+
intLitER :: ExprReg
intLitER = digit $$ kleene digit

-- Identificador: letter(letter|digit)*
identifierER :: ExprReg
identifierER = letter $$ kleene (letter ## digit)

-- Palabras reservadas (literales exactas)
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

-- Operadores y delimitadores
opPlus, opMinus, opTimes, opAssign, opEq, opLeq :: ExprReg
delimSemi, delimLPar, delimRPar :: ExprReg

opPlus   = term '+'
opMinus  = term '-'
opTimes  = term '*'
opAssign = term ':' $$ term '='
opEq     = term '='
opLeq    = term '<' $$ term '='
delimSemi = term ';'
delimLPar = term '('
delimRPar = term ')'

-- Espacios: ( |\t|\n|\r)+
wsER :: ExprReg
wsER = (term ' ' ## term '\t' ## term '\n' ## term '\r')
    $$ kleene (term ' ' ## term '\t' ## term '\n' ## term '\r')

-- *** CAMBIO: AÑADIDA ER DE COMENTARIO ***
-- Comentario: # seguido de cualquier cosa que no sea \n, 0 o más veces
notNewline :: ExprReg
notNewline =
  digit ## lower ## upper ## term ' ' ## term '\t' ## term '\r' ##
  term '!' ## term '"' ## term '$' ## term '%' ## term '&' ##
  term '\'' ## term '(' ## term ')' ## term '*' ## term '+' ##
  term ',' ## term '-' ## term '.' ## term '/' ## term ':' ##
  term ';' ## term '<' ## term '=' ## term '>' ## term '?' ##
  term '@' ## term '[' ## term '\\' ## term ']' ## term '^' ##
  term '_' ## term '`' ## term '{' ## term '|' ## term '}' ## term '~'

commentER :: ExprReg
commentER = term '#' $$ kleene notNewline

-- ============================================
-- ER completa de IMP: unión de todas las ERs
-- ============================================

-- ORDEN IMPORTANTE:
impER :: ExprReg
impER = kwIf ## kwThen ## kwElse ## kwWhile ## kwDo ## kwSkip
      ## kwTrue ## kwFalse ## kwNot ## kwAnd
      ## opAssign ## opLeq -- := y <= antes de otros
      ## opPlus ## opMinus ## opTimes ## opEq
      ## delimSemi ## delimLPar ## delimRPar
      ## identifierER -- id después de keywords
      ## intLitER
      ## wsER
      ## commentER -- *** CAMBIO: AÑADIDO COMENTARIO ***

-- ============================================
-- Función de clasificación (mu)
-- ============================================

-- | Clasifica un lexema en su TokenKind correspondiente
impMu :: String -> Maybe TokenKind
impMu lexema = case lexema of
  -- Palabras reservadas (prioridad alta)
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
  
  -- Operadores
  "+"  -> Just Plus
  "-"  -> Just Minus
  "*"  -> Just Times
  ":=" -> Just Assign
  "="  -> Just Eq
  "<=" -> Just Leq
  
  -- Delimitadores
  ";"  -> Just Semi
  "("  -> Just LPar
  ")"  -> Just RPar
  
  -- Literales, ID, y Comentarios (basados en su forma)
  _ | all isDigit lexema && not (null lexema) -> Just IntLit
  
    -- *** CAMBIO: Lógica de ID mejorada ***
    | not (null lexema) && isLetter (head lexema) && all isAlphaNum (tail lexema) -> Just Id

    | all isSpace lexema && not (null lexema) -> Just WS
    
    -- *** CAMBIO: AÑADIDO RECONOCIMIENTO DE COMENTARIO ***
    | not (null lexema) && head lexema == '#' -> Just Comment
    
    | otherwise -> Nothing  -- Error léxico

  where
    isDigit c = c `elem` "0123456789"
    isLetter c = c `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    isAlphaNum c = isLetter c || isDigit c

-- ============================================
-- Funciones de lexer para IMP
-- ============================================

-- | Construye el MDD de IMP (se hace una sola vez)
buildImpMDD :: MDD
buildImpMDD =
  let -- Pipeline completo: ER -> AFNε -> AFN -> AFD -> AFDmin -> MDD
      afnep  = exprRegToAFNEp impER
      afn    = afnEpToAFN afnep
      afd    = afnToAfd afn
      afdMin = afdToAfdMin afd
      
      -- Mu genérico (luego impMu reclasifica)
      mu = Map.fromList [ (q, Id) | q <- finalesM afdMin ]
      
  in afdMinToMDD afdMin mu

-- | Lexer de IMP - versión con tokens clasificados
-- Esta es la función principal que debes usar
-- *** CAMBIO: Propaga el 'Either' del lexer ***
lexImpTokens :: String -> Either String [Token]
lexImpTokens = buildLexer buildImpMDD impMu