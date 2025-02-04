#!/usr/bin/env sh

set -euo pipefail

mkdir -p metadata
echo 123>metadata/gitinfo

./scripts/make-image-metadata

cabal build

cabal exec site -- clean && cabal exec site -- watch --port 1235
