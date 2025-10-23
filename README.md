# IMP
================================================

## Analizador Léxico en Haskell

Este proyecto implementa, paso a paso, la construcción de autómatas a partir de **expresiones regulares** siguiendo el método de **Thompson**. El objetivo es modelar:

1. **Expresiones Regulares** (`ExprReg`).
2. **Autómatas Finitos No Deterministas con transiciones ε (AFN-ε)**.
3. **Conversión de AFN-ε a AFN** (eliminación de ε).
4. **Conversión de AFN a AFD** (conversión a determinista).
5. **Minimización del AFD**.
6. Integración con un **Lexer**.

---

## 📂 Estructura del Proyecto

```bash
IMP
├── app
│   ├── Main.hs
│   └── MakeGolden.hs
├── imp.cabal
├── program.imp
├── README.md
├── samples
│   ├── expected
│   │   ├── 01_simple.golden
│   │   ├── ...
│   └── imp
│       ├── 01_simple.imp
│       ├── ...
├── scripts
│   └── create_golden.sh
├── specs
│   └── IMP.md
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
│           │   ├── Lenguajes
│           │   │   ├── ImpFast.hs
│           │   │   └── ImpSpec.hs
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

---

## 🖥️ Compilación
```bash
cabal update
cabal build
```
---

## 🖥️ Ejecución

Se puede ejecutar el lexer mediante linea de comandos
```bash
cabal run imp -- "while x <= 10 do x := x + 1;"
```
También se puede ejecutar con un archivo .imp como argumento con la bandera -f
```bash
cabal run imp -- -p -f program.imp
```
**Nota:** la bandera ''-p'' muestra el proceso completo de transformaciones en terminal

---

##  Pruebas (golden tests)

Este repo usa **golden tests** con Hspec.  
Cada archivo `samples/imp/*.imp` tiene su salida esperada en `samples/expected/*.golden`  
(una línea por token con `show`). El generador utiliza **ImpFast**.

### 1. Generar/actualizar los `.golden`
Si es la primera vez (o cambiaste la salida del lexer):

```bash
cabal build
./scripts/create_golden.sh
```

### 2. Ejecutar los tests
```bash
cabal test

# Opcionales:
cabal test --test-options="--format=progress"   # ver cada caso
cabal test --test-options="--fail-fast"         # detener en el primer fallo
```

### 3. Agregar un nuevo caso
1. Crea el archivo fuente:
   ```bash
   echo "if x then y := 1 else y := 2" > samples/imp/15_if_assign.imp
   ```
2. Genera su `.golden`:
   ```bash
   ./scripts/create_golden.sh
   ```
3. Ejecuta los tests:
   ```bash
   cabal test
   ```

### 4. Estructura relevante para pruebas
```
samples/
  imp/         # entradas .imp
  expected/    # salidas .golden
scripts/
  create_golden.sh        # genera/actualiza todos los .golden
app/
  MakeGolden.hs           # ejecutable: imprime tokens con ImpFast
test/
  Spec.hs                 # compara .imp vs .golden (ordenado y 1 example por archivo)
```


## 🧾 Documentación de las pruebas
La explicación conceptual de cada caso (qué verifica, reglas del lexer, prefijo máximo, ignorables, etc.) está en:
```
specs/IMP.md
```
