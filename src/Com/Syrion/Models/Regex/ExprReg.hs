-- src/Com/Syrion/Models/Regex/ExprReg.hs
module Com.Syrion.Models.Regex.ExprReg

-- Tipo de dato principal
  (
    ExprReg(..)
  , epsilon
  , term
  , ($$)        -- concatenación (and)
  , (##)        -- disyunción (or)
  , kleene      -- estrella de Kleene
  ) where

-- Expresión regular: ε, símbolo, concatenación, disyunción y estrella de Kleene.
data ExprReg
  = Epsilon
  | Term Char
  | And ExprReg ExprReg
  | Or  ExprReg ExprReg
  | Kleene ExprReggex
  deriving (Eq)

-- Pretty-print: Representación como cadena (String).
instance Show ExprReg where
  show Epsilon    = "ε"
  show (Term c)   = [c]
  show (Kleene r) = parent (show r) ++ "*"
  show (And l r)  = parent (show l) ++ parent (show r)
  show (Or  l r)  = parent (show l) ++ "+" ++ parent (show r)


-- ==========================
--  Constructores auxiliares
-- ==========================

-- | ε
epsilon :: ExprReg
epsilon = Epsilon

-- | Un símbolo terminal.
term :: Char -> ExprReg
term = Term

-- | Concatenación.
infixl 7 $$
($$) :: ExprReg -> ExprReg -> ExprReg
($$) = And

-- | Disyunción.
infixl 6 ##
(##) :: ExprReg -> ExprReg -> ExprReg
(##) = Or

-- | Estrella de Kleene.
kleene :: ExprReg -> ExprReg
kleene = Kleene


-- ==========================
--  Helper local
-- ==========================

-- Envuelve con paréntesis solo si la subexpresión no es atómica.
parent :: String -> String
parent s =
  case s of
    "ε" -> s
    [c] -> [c]
    _   -> "(" ++ s ++ ")"
