#!/bin/bash
set -o errexit
set -o nounset

runghc -threaded runtests.hs -j4
