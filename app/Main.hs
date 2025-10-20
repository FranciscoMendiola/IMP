-- app/Main.hs
module Main where

import System.IO (hSetEncoding, stdout, utf8)
import qualified Data.Map.Strict as Map

-- ER DSL
import Com.Syrion.Models.Regex.ExprReg
  ( ExprReg(..), term, kleene, (##), ($$) )

-- Pipeline AFNε -> AFN -> AFD
import Com.Syrion.Models.Automata.AFNEp (exprRegToAFNEp)
import Com.Syrion.Models.Automata.AFN   (afnEpToAFN)
import Com.Syrion.Models.Automata.AFD   (AFD(..), afnToAfd)

-- MDD
import Com.Syrion.Models.Automata.MDD
  ( MDD(..), afdToMDD, longestPrefixMDD )

-- Clasificación de tokens
import Com.Syrion.Models.Lexer.Token (TokenKind(..))
import Com.Syrion.Models.Lenguajes.ImpSpec (impMu)  -- tu función de clasificación

-- =========================
-- ERs pequeñas por grupo
-- =========================

digit :: ExprReg
digit = foldr1 (##) (map term "0123456789")

lower :: ExprReg
lower = foldr1 (##) (map term "abcdefghijklmnopqrstuvwxyz")

upper :: ExprReg
upper = foldr1 (##) (map term "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

letter :: ExprReg
letter = lower ## upper

-- Id: letter (letter|digit)*
identifierER :: ExprReg
identifierER = letter $$ kleene (letter ## digit)

-- Int: digit+
intLitER :: ExprReg
intLitER = digit $$ kleene digit

-- WS: (espacio|tab|nl)+
wsER :: ExprReg
wsER = (term ' ' ## term '\t' ## term '\n')
    $$ kleene (term ' ' ## term '\t' ## term '\n')

-- Keywords literales: if, then, else, while, do, skip, true, false, not, and
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

-- Unimos KW + Id en un solo MDD (clasificación final via impMu con prioridad a KW)
kwPlusIdER :: ExprReg
kwPlusIdER = kwIf ## kwThen ## kwElse ## kwWhile ## kwDo ## kwSkip
          ## kwTrue ## kwFalse ## kwNot ## kwAnd
          ## identifierER

-- Operadores/delims de 1 char (SIN ':=' ni '<=' aquí)
ops1ER :: ExprReg
ops1ER =
  foldr1 (##) (map term "+-*=;()<>:")  -- incluimos ':', '<' y '=' unitarios

-- =========================
-- Builders por grupo (AFD -> MDD, sin minimizar)
-- =========================

buildMDD :: ExprReg -> MDD
buildMDD er =
  let afnep = exprRegToAFNEp er
      afn   = afnEpToAFN afnep
      afd   = afnToAfd afn
      mu    = Map.fromList
               [ (i, Id)
               | (i, qD) <- zip [0..] (estadosD afd)
               , qD `elem` finalesD afd
               ]
  in afdToMDD afd mu

mddKW_ID :: MDD
mddKW_ID = buildMDD kwPlusIdER

mddINT :: MDD
mddINT = buildMDD intLitER

mddWS :: MDD
mddWS = buildMDD wsER

mddOPS1 :: MDD
mddOPS1 = buildMDD ops1ER

-- =========================
-- Escáner combinado (prefijo máximo + prioridad)
-- =========================

-- Prioridad cuando hay empates de longitud:
--   1) KW/Id  2) Int  3) Ops1  4) WS
priorityOrder :: [(String, MDD)]
priorityOrder =
  [ ("KW_ID", mddKW_ID)
  , ("INT"  , mddINT)
  , ("OPS1" , mddOPS1)
  , ("WS"   , mddWS)
  ]

data Tok = Tok { kind :: TokenKind, lexeme :: String, pos :: Int }
  deriving (Show)

scanIMP :: String -> Either String [Tok]
scanIMP = go 0
  where
    go _ [] = Right []
    go i s@(c:_) =
      let candidates =
            [ (tag, longestPrefixMDD m s, m)
            | (tag, m) <- priorityOrder
            ]
          (bestTag, bestLex, _bestM) =
            foldl pick ("", "", mddWS) candidates
          pick acc@(_, accLex, _) cur@(tag, lex, m)
            | length lex > length accLex = cur
            | otherwise                   = acc

          rest = drop (length bestLex) s

          -- Recombinar pares 2-char antes de clasificar
          (lexToClassify, extraConsumed) =
            case (bestTag, bestLex, rest) of
              ("OPS1", "<", ('=':_)) -> ("<=", 1)  -- Leq
              ("OPS1", ":", ('=':_)) -> (":=", 1)  -- Assign
              _                      -> (bestLex, 0)

          totalLen = length bestLex + extraConsumed
          advance  = drop totalLen s
      in if null bestLex
           then Left $ "Error lexico en la posicion " ++ show i ++ ": '" ++ take 1 s ++ "'"
           else
             case impMu lexToClassify of
               Just WS      -> go (i + totalLen) advance   -- ignoramos espacios
               Just Comment -> go (i + totalLen) advance   -- por si luego añades comentarios
               Just knd     ->
                 let t = Tok { kind = knd, lexeme = lexToClassify, pos = i }
                 in (t :) <$> go (i + totalLen) advance
               Nothing      ->
                 Left $ "Error lexico en la posicion " ++ show i ++ ": '" ++ lexToClassify ++ "'"

-- =========================
-- Main
-- =========================

main :: IO ()
main = do
  hSetEncoding stdout utf8
  putStrLn "=== LEXER IMP (multi-MDD, recombinando <= y :=) ==="
  let prog = "if"
  case scanIMP prog of
    Left err   -> putStrLn ("LEX ERROR: " ++ err)
    Right toks -> mapM_ print toks
