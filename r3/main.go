package main

import (
	"fmt"
	"os"
)

// Reports an error and exit.
func fail(format string, args ...any) {
	fmt.Fprintf(os.Stderr, format+"\n", args...)
	os.Exit(1)
}

func main() {
	if len(os.Args) != 2 {
		fail("%s: invalid number of arguments", os.Args[0])
	}

	tok := tokenize(os.Args[1])

	fmt.Printf("  .globl main\n")
	fmt.Printf("main:\n")

	// The first token must be a number
	fmt.Printf("  li a0, %d\n", tok.getNumber())
	tok = tok.next

	// ... followed by either `+ <number>` or `- <number>`.
	for tok.kind != TK_EOF {
		if tok.equal("+") {
			fmt.Printf("  addi a0, a0, %d\n", tok.next.getNumber())
			tok = tok.next.next
			continue
		}

		tok = tok.skip("-")
		fmt.Printf("  addi a0, a0, -%d\n", tok.getNumber())
		tok = tok.next
	}

	fmt.Printf("  ret\n")
}
