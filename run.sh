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
runghc -threaded runtests.hs
