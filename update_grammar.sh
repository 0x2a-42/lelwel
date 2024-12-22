#!/bin/sh

cargo run --features=cli --bin=llw -- -o src/frontend src/frontend/lelwel.llw && rustfmt src/frontend/generated.rs
