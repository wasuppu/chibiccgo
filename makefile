TEST_DIR = test/$(ARCH)
TEST_SRCS := $(wildcard test/*.c)
TESTS := $(patsubst test/%.c,$(TEST_DIR)/%.exe,$(TEST_SRCS))

ifeq ($(ARCH),riscv)
    CC = riscv64-unknown-elf-gcc
	RUN = qemu-riscv64 -L /opt/riscv-linux/sysroot
else
    CC = gcc
    RUN =
endif

chibicc:
	go build -o chibicc *.go

$(TEST_DIR)/%.exe: chibicc test/%.c
	mkdir -p $(TEST_DIR)
	$(CC) -o- -E -P -C test/$*.c | ./chibicc -march=$(ARCH) -o $(TEST_DIR)/$*.s -
	$(CC) -o $@ $(TEST_DIR)/$*.s -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; $(RUN) ./$$i || exit 1; echo; done
	test/driver.sh

x64:
	$(MAKE) test ARCH=x64

riscv:
	$(MAKE) test ARCH=riscv

testall: x64 riscv

clean:
	rm -rf chibicc tmp* test/x64 test/riscv
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: x64 riscv test testall clean
