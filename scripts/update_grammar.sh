#!/bin/sh

cargo run --features=cli --bin=llw -- -o ../src/frontend ../src/frontend/lelwel.llw && rustfmt --edition 2024 ../src/frontend/generated.rs
