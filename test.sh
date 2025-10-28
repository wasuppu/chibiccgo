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

assert 0 'return 0;'
assert 42 'return 42;'
assert 21 'return 5+20-4;'
assert 41 'return  12 + 34 - 5 ;'
assert 47 'return 5+6*7;'
assert 15 'return 5*(9-6);'
assert 4 'return (3+5)/2;'

# c6
assert 10 'return -10+20;'
assert 10 'return - -10;'
assert 10 'return - - +10;'

# c7
assert 0 'return 0==1;'
assert 1 'return 42==42;'
assert 1 'return 0!=1;'
assert 0 'return 42!=42;'

assert 1 'return 0<1;'
assert 0 'return 1<1;'
assert 0 'return 2<1;'
assert 1 'return 0<=1;'
assert 1 'return 1<=1;'
assert 0 'return 2<=1;'

assert 1 'return 1>0;'
assert 0 'return 1>1;'
assert 0 'return 1>2;'
assert 1 'return 1>=0;'
assert 1 'return 1>=1;'
assert 0 'return 1>=2;'

# c9
assert 1 'return 1; 2; 3;'
assert 2 '1; return 2; 3;'
assert 3 '1; 2; return 3;'

# c10
assert 3 'a=3; return a;'
assert 8 'a=3; z=5; return a+z;'
assert 6 'a=b=3; return a+b;'

# c11
assert 3 'foo=3; return foo;'
assert 8 'foo123=3; bar=5; return foo123+bar;'

# c12
assert 1 'return 1; 2; 3;'
assert 2 '1; return 2; 3;'
assert 3 '1; 2; return 3;'

echo OK
