#!/bin/bash

set -e

tag="$1"

# Normally I would build inside docker, but building haskell inside docker takes ages --> build extern and copy binaries
stack install --local-bin-path ./dist
docker build . -t "battleskell:$tag"
docker tag "battleskell:$tag" "ghcr.io/johnnycrazy/battleskell:$tag"
docker push "ghcr.io/johnnycrazy/battleskell:$tag"
