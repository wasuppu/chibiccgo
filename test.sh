#!/bin/bash

# use with ./test r[n]

if [[ $1 =~ ^r[0-9]+$ ]]; then
    make rvcc $1 || exit 1
fi

if ! command -v ./rvcc &> /dev/null; then
    echo "Error: rvcc not found in current directory"
    exit 1
fi

assert() {
  expected=$1
  input=$2

  ./rvcc "$input" > tmp.s || exit
  riscv64-unknown-elf-gcc -static -o tmp tmp.s
  qemu-riscv64 -L $RISCV/sysroot ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

run_tests() {
    local level=${1#r}
    for ((i=1; i<=level; i++)); do
        func="test$i"
        if declare -F "$func" > /dev/null; then
            $func || exit 1
        fi
    done
}

test1() {
    assert 0 0
    assert 42 42
}

test2() {
    assert 21 '5+20-4'
}

test3() {
    assert 41 ' 12 + 34 - 5 '
}

test5() {
    assert 47 '5+6*7'
    assert 15 '5*(9-6)'
    assert 4 '(3+5)/2'
}

if [[ $1 =~ ^r[0-9]+$ ]]; then
    run_tests "$1"
else
    echo "Error: target must be like rn, $1 is not support"
    exit 1
fi

echo OK
