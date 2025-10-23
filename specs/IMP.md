# Especificación del Lexer IMP

Este documento describe qué verifica el conjunto de pruebas y cómo debe comportarse el lexer de IMP a nivel conceptual. Los tests en `test/Spec.hs` usan **golden tests** para validar que la tokenización cumpla con estas reglas.

---

## 1. Lenguaje IMP (resumen)

- **Identificadores (`Id`)**: letras (`a–z`, `A–Z`) seguidas de letras, dígitos o `_`. Deben **distinguirse** de palabras reservadas.
- **Enteros (`IntLit`)**: una o más cifras decimales `0–9` (sin signo aquí; el `-` se tokeniza como operador).
- **Palabras reservadas**: `if`, `then`, `else`, `while`, `do`, `skip`, `true`, `false`, `not`, `and`.
- **Operadores / símbolos** (ejemplos comunes): `+`, `-`, `*`, `:=`, `=`, `<=`, `;`, `(`, `)`.
- **Ignorables**: espacios, tabs, saltos de línea; comentarios (según la gramática de la práctica).

> Nota: La lista exacta de `TokenKind` está en `src/Com/Syrion/Models/Lexer/Token.hs`.

---

## 2. Reglas del lexer

1. **Prefijo máximo (maximal munch)**  
   Siempre consumir el **lexema más largo** posible que forme un token válido.  
   Ej.: al ver `<=` no debe partirse en `<` y `=`, sino producir un único token `Leq`.

2. **Distinción de reservadas vs identificadores**  
   Un lexema que coincida con una palabra reservada debe tokenizarse como reservada; en otro caso, `Id`.

3. **Ignorables**  
   Espacios, tabs y saltos de línea no generan tokens. Los comentarios se omiten por completo.

4. **Errores léxicos**  
   Si aparece un carácter o secuencia desconocida, el lexer debe **reportar error** (los golden no contemplan errores; esos casos fallarán la prueba).

---

## 3. Estrategia de pruebas (golden tests)

- Para cada `samples/imp/<caso>.imp`, existe `samples/expected/<caso>.golden`.
- El archivo `.golden` contiene **una línea por token** usando `show` del tipo `Token`.
- El test ejecuta el lexer **ImpFast** sobre el `.imp` y compara la salida **exactamente** contra el `.golden`.

### Flujo de trabajo
1. Generar/actualizar esperados: `./scripts/create_golden.sh` (usa `make-golden`).
2. Ejecutar pruebas: `cabal test`.
3. Si cambias el formato o la semántica del lexer de forma **intencional**, vuelve a generar los `.golden`.

---

## 4. Casos de prueba actuales

> Los nombres de archivo indican el caso que hace. Todos se ubican en `samples/imp/`.

- `01_simple.imp` — Programa mínimo con asignaciones sencillas.
- `02_if_then_else.imp` — Estructura condicional y reservadas.
- `03_while.imp` — Bucle `while` con cuerpo.
- `04_arithmetic.imp` — Operadores aritméticos `+`, `-`, `*` y paréntesis.
- `05_boolean.imp` — Booleanos `true/false`, `not`, `and`, comparaciones.
- `06_comments.imp` — Comentarios e ignorables.
- `07_whitespace.imp` — Mezcla de espacios, tabs, nuevas líneas.
- `08_maximal_munch.imp` — Casos límite de prefijo máximo (e.g., `:=`, `<=`, ids largos).
- `09_complex.imp` — Programa con varios constructos combinados.
- `10_nested.imp` — Anidamiento de `if`/`while` y paréntesis.
- `11_comparisons.imp` — Igualdad, menor/igual, otros relacionales contemplados.
- `12_sequential.imp` — Secuencia de instrucciones con `;`.
- `13_parentheses.imp` — Énfasis en agrupamiento y precedencia.
- `14_all_operators.imp` — Barrido sobre operadores y delimitadores soportados.

> Al agregar un nuevo caso `NN_nombre.imp`, recuerda generar `NN_nombre.golden` con el script.

---

## 5. Criterios verificados por los tests

- **Reconocimiento correcto** de cada categoría léxica (`TokenKind`) y su **lexema**.
- **Aplicación del prefijo máximo** en operadores y tokens compuestos.
- **Omisión de ignorables** (no deben aparecer tokens de whitespace/comentarios).
- **Consistencia**: la misma entrada produce la misma secuencia de tokens (salida estable).

---

## 6. Enlaces de implementación

- Lexer rápido: `src/Com/Syrion/Models/Lenguajes/ImpFast.hs`
- ImpSpec / MDD: `src/Com/Syrion/Models/Lenguajes/ImpSpec.hs`, `src/Com/Syrion/Models/Automata/MDD.hs`
- Tokens: `src/Com/Syrion/Models/Lexer/Token.hs`
- Test runner: `test/Spec.hs`
- Generador de golden: `app/MakeGolden.hs` + `scripts/create_golden.sh`

---

**Conclusión**: Los golden garantizan que la tokenización de IMP se mantenga estable y conforme a las reglas del lexer (reservadas, identificadores, operadores compuestos, ignorables y prefijo máximo). Cualquier cambio no intencional en la salida se detecta automáticamente al correr `cabal test`.
