-- src/Com/Syrion/Models/Lenguajes/ImpFast.hs
module Com.Syrion.Models.Lenguajes.ImpFast
  ( scanIMP
  , renderTokens
  , renderToken
  ) where

import qualified Data.Map.Strict as Map

import Com.Syrion.Models.Regex.ExprReg
  ( ExprReg(..), term, kleene, (##), ($$) )

import Com.Syrion.Models.Automata.AFNEp (exprRegToAFNEp)
import Com.Syrion.Models.Automata.AFN   (afnEpToAFN)
import Com.Syrion.Models.Automata.AFD   (AFD(..), afnToAfd)

import Com.Syrion.Models.Automata.MDD
  ( MDD(..), afdToMDD, longestPrefixMDD )

import Com.Syrion.Models.Lexer.Token   (TokenKind(..))
import Com.Syrion.Models.Lexer.Lexer   (Token(..))
import Com.Syrion.Models.Lenguajes.ImpSpec (impMu)

-- ===== Helpers de prefijo máximo (manuales) =====

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

-- Comentario de línea: '#' seguido de todo hasta antes de '\n'
maslargocomentario :: String -> String
maslargocomentario ('#':cs) =
  let (cuerpo, _) = span (/= '\n') cs
  in '#':cuerpo
maslargocomentario _ = ""

-- ===== MDDs pequeños: WS y operadores 1-char =====

espaciosER :: ExprReg
espaciosER = (term ' ' ## term '\t' ## term '\n')
    $$ kleene (term ' ' ## term '\t' ## term '\n')

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

-- ===== Selección por prioridad (prefijo más largo) =====
-- Orden:
--   1) COMENTARIO  2) ID/KW  3) INT  4) OPS1  5) WS
evaluarPrioridad :: String -> [(String, String)]
evaluarPrioridad s =
  [ ("COM",  maslargocomentario s)
  , ("IDKW", maslargoidokw s)
  , ("INT" , maslargoentero s)
  , ("OPS1", longestPrefixMDD mddOPS1 s)
  , ("WS"  , longestPrefixMDD mddWS   s)
  ]

-- Recombinar <= y := cuando elegimos OPS1 y hay '=' después
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
      let cs = evaluarPrioridad s
          escoger (mejorEtiq, mejorLex) (etiq, lex)
            | length lex > length mejorLex = (etiq, lex)
            | otherwise                    = (mejorEtiq, mejorLex)
          (mejorEtiq, mejorLex) = foldl escoger ("", "") cs
      in if null mejorLex
           then Left $ "Error lexico en la posicion " ++ show pos ++ ": '" ++ [c] ++ "'"
           else
             let resto0                 = drop (length mejorLex) s
                 (lexParaClasificar, k) = recombinar2 mejorEtiq mejorLex resto0
                 total                  = length mejorLex + k
                 avanzar                = drop total s
             in case impMu lexParaClasificar of
                  Just WS   -> recorrer (pos + total) avanzar
                  -- Comentarios SÍ se devuelven como tokens:
                  Just tipo ->
                    let t = Token { tokenKind = tipo, lexeme = lexParaClasificar, position = pos }
                    in (t :) <$> recorrer (pos + total) avanzar
                  Nothing   ->
                    Left $ "Error lexico en la posicion " ++ show pos ++ ": '" ++ lexParaClasificar ++ "'"

-- ===== Render =====

renderToken :: Token -> String
renderToken t =
  let posStr   = show (position t)
      tipoStr  = show (tokenKind t)
      lexStr   = lexeme t
      wPos  = 8
      wTipo = 10
      rellenarIzq n s = replicate (max 0 (n - length s)) ' ' ++ s
      rellenarDer n s = s ++ replicate (max 0 (n - length s)) ' '
  in  rellenarIzq wPos posStr ++ " | "
   ++ rellenarDer wTipo tipoStr ++ " | "
   ++ lexStr

renderTokens :: [Token] -> IO ()
renderTokens ts = do
  let encabezado = "Posición | Tipo      | Lexema"
      subrayado  = replicate (length encabezado) '-'
  putStrLn encabezado
  putStrLn subrayado
  mapM_ (putStrLn . renderToken) ts