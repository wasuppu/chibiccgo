chibicc:
	go build -o chibicc *.go

x64: chibicc
	./test.sh "x64"

riscv: chibicc
	./test.sh "riscv"

test: x64 riscv
	./test-driver.sh

clean:
	rm -f chibicc *.o *~ tmp*

.PHONY: x64 riscv test clean
