#!/bin/bash

grammars=$(find ../examples ../src -name "*.llw")
for f in $grammars; do
  cargo run --features=cli --bin=llw -- -f "$f"
done
