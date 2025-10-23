#!/usr/bin/env bash
set -euo pipefail

SAMPLES="samples/imp"
OUTDIR="samples/expected"
mkdir -p "$OUTDIR"

echo "[golden] Generando .golden..."
for f in "$SAMPLES"/*.imp; do
  base="$(basename "$f" .imp)"
  cabal run exe:make-golden -- "$f" > "$OUTDIR/$base.golden"
  echo "  ✓ $base.golden"
done
