-- src/Com/Syrion/Models/Lenguajes/ImpFast.hs
module Com.Syrion.Models.Lenguajes.ImpFast
  ( scanIMP          -- programa -> tokens (versión rápida/estable)
  , renderTokens     -- imprime tabla: Posición | Tipo | Lexema
  , renderToken      -- imprime una línea
  ) where

import qualified Data.Map.Strict as Map

-- ER DSL (seguimos usando MDD para WS y ops)
import Com.Syrion.Models.Regex.ExprReg
  ( ExprReg(..), term, kleene, (##), ($$) )

-- Pipeline AFNε -> AFN -> AFD
import Com.Syrion.Models.Automata.AFNEp (exprRegToAFNEp)
import Com.Syrion.Models.Automata.AFN   (afnEpToAFN)
import Com.Syrion.Models.Automata.AFD   (AFD(..), afnToAfd)

-- MDD + util prefijo (para WS y operadores/delims)
import Com.Syrion.Models.Automata.MDD
  ( MDD(..), afdToMDD, longestPrefixMDD )

-- Clasificación / tipos de token
import Com.Syrion.Models.Lexer.Token   (TokenKind(..))
import Com.Syrion.Models.Lexer.Lexer   (Token(..))
import Com.Syrion.Models.Lenguajes.ImpSpec (impMu)

--------------------------------------------------------------------------------
-- Escáneres para Id/KW e Int (prefijo máximo)
--------------------------------------------------------------------------------

esLetra :: Char -> Bool
esLetra c = ('a'<=c && c<='z') || ('A'<=c && c<='Z')

esDigito :: Char -> Bool
esDigito c = '0'<=c && c<='9'

esAlfaNum :: Char -> Bool
esAlfaNum c = esLetra c || esDigito c

maslargoidokw :: String -> String
maslargoidokw (c:cs) | esLetra c =
  let (resto, _) = span esAlfaNum cs
  in c:resto
maslargoidokw _ = ""

maslargoentero :: String -> String
maslargoentero (c:cs) | esDigito c =
  let (resto, _) = span esDigito cs
  in c:resto
maslargoentero _ = ""

--------------------------------------------------------------------------------
-- MDDs pequeños: sólo WS y operadores/delims de 1 char
--------------------------------------------------------------------------------

-- WS: (espacio|tab|nl)+
espaciosER :: ExprReg
espaciosER = (term ' ' ## term '\t' ## term '\n')
    $$ kleene (term ' ' ## term '\t' ## term '\n')

-- Operadores/delims de 1 char (SIN meter aquí ':=' ni '<='; los recombinamos)
ops1ER :: ExprReg
ops1ER = foldr1 (##) (map term "+-*=;()<>:")

construirMDD :: ExprReg -> MDD
construirMDD er =
  let afnep = exprRegToAFNEp er
      afn   = afnEpToAFN afnep
      afd   = afnToAfd afn
      mu    = Map.fromList
               [ (i, Id)
               | (i, qD) <- zip [0..] (estadosD afd)
               , qD `elem` finalesD afd
               ]
  in afdToMDD afd mu

mddWS   :: MDD;   mddWS   = construirMDD espaciosER
mddOPS1 :: MDD;   mddOPS1 = construirMDD ops1ER

-- Prioridad en empates de longitud:
--   1) Id/KW (manual)  2) Int (manual)  3) Ops1 (MDD)  4) WS (MDD)
evaluarPrioridad :: String -> [(String, String)]
evaluarPrioridad s =
  [ ("IDKW", maslargoidokw s)
  , ("INT" , maslargoentero   s)
  , ("OPS1", longestPrefixMDD mddOPS1 s)
  , ("WS"  , longestPrefixMDD mddWS   s)
  ]

-- Recombina '<' '=' -> "<=" y ':' '=' -> ":=" si el elegido fue OPS1 y hay '=' después
recombinar2 :: String -> String -> String -> (String, Int)
recombinar2 etiqueta lex resto =
  case (etiqueta, lex, resto) of
    ("OPS1", "<", ('=':_)) -> ("<=", 1)
    ("OPS1", ":", ('=':_)) -> (":=", 1)
    _                      -> (lex , 0)

scanIMP :: String -> Either String [Token]
scanIMP = recorrer 0
  where
    recorrer _ [] = Right []
    recorrer pos s@(c:_) =
      -- evaluar en orden de prioridad
      let cs = evaluarPrioridad s

          -- elegir el más largo; si empatan, gana el primero en la lista (prioridad)
          escoger (mejorEtiq, mejorLex) (etiq, lex)
            | length lex > length mejorLex = (etiq, lex)
            | otherwise                    = (mejorEtiq, mejorLex)

          (mejorEtiq, mejorLex) = foldl escoger ("", "") cs

      in if null mejorLex
           then Left $ "Error lexico en la posicion " ++ show pos ++ ": '" ++ [c] ++ "'"
           else
             let resto0          = drop (length mejorLex) s
                 (lexParaClasificar, k) = recombinar2 mejorEtiq mejorLex resto0
                 total           = length mejorLex + k
                 avanzar         = drop total s
             in case impMu lexParaClasificar of
                  Just WS      -> recorrer (pos + total) avanzar
                  Just Comment -> recorrer (pos + total) avanzar
                  Just tipo    ->
                    let t = Token { tokenKind = tipo, lexeme = lexParaClasificar, position = pos }
                    in (t :) <$> recorrer (pos + total) avanzar
                  Nothing      ->
                    Left $ "Error lexico en la posicion " ++ show pos ++ ": '" ++ lexParaClasificar ++ "'"

-- | Renderiza un token como una línea:
--   "   12 | KIf       | if"
renderToken :: Token -> String
renderToken t =
  let posStr   = show (position t)
      tipoStr  = show (tokenKind t)
      lexStr   = lexeme t
      -- anchos por defecto (ajústalos si quieres)
      wPos  = 8
      wTipo = 10
      rellenarIzq n s  = replicate (max 0 (n - length s)) ' ' ++ s
      rellenarDer n s = s ++ replicate (max 0 (n - length s)) ' '
  in  rellenarIzq wPos posStr ++ " | "
   ++ rellenarDer wTipo tipoStr ++ " | "
   ++ lexStr

-- | Imprime una tabla con encabezados en español:
--   "Posición | Tipo      | Lexema"
renderTokens :: [Token] -> IO ()
renderTokens ts = do
  let encabezado = "Posición | Tipo      | Lexema"
      subrayado  = replicate (length encabezado) '-'
  putStrLn encabezado
  putStrLn subrayado
  mapM_ (putStrLn . renderToken) ts