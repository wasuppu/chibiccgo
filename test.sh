#!/bin/bash

arch=$1
case $arch in
    "x64")
        build_cmd="gcc -static -o tmp tmp.s"
        run_cmd="./tmp"
        ;;
    "riscv")
        build_cmd="riscv64-unknown-elf-gcc -static -o tmp tmp.s"
        run_cmd="qemu-riscv64 -L \$RISCV/sysroot ./tmp"
        ;;
    *)
        echo "Error: unsupported architecture $arch"
        exit 1
        ;;
esac


assert() {
    expected=$1
    input=$2

    ./chibicc $arch "$input" > tmp.s || exit
    eval $build_cmd
    eval $run_cmd
    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo "$input => $actual"
    else
        echo "$input => $expected expected, but got $actual"
        exit 1
    fi
}

assert 0 0
assert 42 42
assert 21 '5+20-4'
assert 41 ' 12 + 34 - 5 '
assert 47 '5+6*7'
assert 15 '5*(9-6)'
assert 4 '(3+5)/2'

# c6
assert 10 '-10+20'
assert 10 '- -10'
assert 10 '- - +10'

echo OK
