# IMP
# ================================================================

# Analizador Léxico en Haskell

Este proyecto implementa, paso a paso, la construcción de autómatas a partir de **expresiones regulares** siguiendo el método de **Thompson**. El objetivo es modelar:

1. **Expresiones Regulares** (`ExprReg`).
2. **Autómatas Finitos No Deterministas con transiciones ε (AFN-ε)**.
3. **Conversión de AFN-ε a AFN** (eliminación de ε).
4. **Conversión de AFN a AFD** (determinización).
5. **Minimización del AFD**.
6. Integración con un **Lexer**.

---

## 📂 Estructura del Proyecto

```bash
IMP  
.
├── app
│   └── Main.hs
├── imp.cabal
├── README.md
├── src
│   └── Com
│       └── Syrion
│           ├── Models
│           │   ├── Automata
│           │   │   ├── AFD.hs
│           │   │   ├── AFDMin.hs
│           │   │   ├── AFNEp.hs
│           │   │   ├── AFN.hs
│           │   │   └── MDD.hs
│           │   ├── Lexer
│           │   │   ├── Lexer.hs
│           │   │   └── Token.hs
│           │   └── Regex
│           │       └── ExprReg.hs
│           └── Utils
│               └── Util.hs
└── test
    └── Spec.hs
```
---

## ⚙️ Dependencias

- **GHCup** (instala GHC + Cabal + HLS)
- GHC ≥ 9.0
- Cabal ≥ 3.8

En Debian/Ubuntu:
```bash
sudo apt update
sudo apt install curl build-essential libffi-dev libgmp-dev libncurses-dev zlib1g-dev
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

## 🖥️ Ejecución
```bash
cabal update
cabal build
cabal run 
```