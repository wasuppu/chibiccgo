TEST_DIR = test/$(ARCH)
TEST_SRCS := $(wildcard test/*.c)
TESTS := $(patsubst test/%.c,$(TEST_DIR)/%.exe,$(TEST_SRCS))

ifeq ($(ARCH),riscv)
    CC = riscv64-unknown-linux-gnu-gcc
	RUN = qemu-riscv64 -L /opt/riscv-linux/sysroot
	FLAG = -static
else
    CC = gcc
    RUN =
	FLAG =
endif

chibicc:
	go build -o chibicc *.go

# Stage 1

$(TEST_DIR)/%.exe: chibicc test/%.c
	mkdir -p $(TEST_DIR)
	$(CC) -o- -E -P -C test/$*.c | ./chibicc -march=$(ARCH) -o $(TEST_DIR)/$*.s -
	$(CC) $(FLAG) -o $@ $(TEST_DIR)/$*.s -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; $(RUN) ./$$i || exit 1; echo; done
	test/driver.sh ./chibicc

# Stage 2

CFLAGS=-std=c11 -g -fno-common
SCSRCS=$(wildcard ./source/*.c)
SCOBJS=$(notdir $(SCSRCS:.c=.o))

stage2/chibicc: $(SCOBJS:%=stage2/%)
	$(CC) $(CFLAGS) $(FLAG) -o $@ $^ $(LDFLAGS)

stage2/%.s: chibicc self.py ./source/%.c
	mkdir -p stage2/test
	./self.py ./source/chibicc.h ./source/$*.c > stage2/$*.c
	./chibicc -march=$(ARCH) -o stage2/$*.s stage2/$*.c

stage2/%.o: stage2/%.s
	$(CC) -c stage2/$*.s -o stage2/$*.o

stage2/test/%.exe: stage2/chibicc test/%.c
	mkdir -p stage2/test
	gcc -o- -E -P -C test/$*.c | $(RUN) ./stage2/chibicc -o stage2/test/$*.s -
	gcc -o $@ stage2/test/$*.s -xc test/common

test-stage2: $(patsubst $(TEST_DIR)/%.exe,stage2/test/%.exe,$(TESTS))
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	test/driver.sh ./stage2/chibicc

x64:
	$(MAKE) clean
	$(MAKE) test ARCH=x64
	$(MAKE) test-stage2 ARCH=x64

riscv:
	$(MAKE) clean
	$(MAKE) test ARCH=riscv
	$(MAKE) test-stage2 ARCH=riscv

# Stage 2

testall: x64 riscv

clean:
	rm -rf chibicc tmp* test/x64 test/riscv stage2
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: x64 riscv test testall clean
