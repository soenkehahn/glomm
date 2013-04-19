#!/bin/bash
set -o errexit
set -o nounset

ghc -fext-core F.hs
cat F.hcr
echo ========================
runhaskell glomm.hs F.hcr > F.js
cat F.js
echo ========================
nodejs F.js
