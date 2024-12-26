#!/bin/bash

export RUSTFLAGS="--remap-path-prefix $HOME=~"

mkdir parsers

for f in ../examples/*; do
  wasm-pack build -t web "$f"
  cp "$f"/pkg/*.js "$f"/pkg/*_bg.wasm parsers
done

wasm-pack build -t web .. --features=wasm
cp ../pkg/*.js ../pkg/*_bg.wasm parsers
