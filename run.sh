#!/bin/bash
set -o errexit
set -o nounset

# ~ ghc -fext-core F.hs -hide-all-packages -S
# ~ cat F.hcr
# ~ echo ========================
# ~ runhaskell glomm.hs F.hcr > F.js
# ~ cat F.js
# ~ echo ========================
# ~ nodejs F.js

rm -rf _make
mkdir _make
cp baseCoreFiles/* _make -rv
runghc -threaded runtests.hs
