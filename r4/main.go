package main

import (
	"fmt"
	"os"
)

var source string
var currentInputLoc int

// Reports an error and exit.
func fail(format string, args ...any) {
	fmt.Fprintf(os.Stderr, format+"\n", args...)
	os.Exit(1)
}

// Reports an error location and exit.
func failAt(loc int, format string, args ...any) {
	pos := currentInputLoc - loc
	fmt.Fprintln(os.Stderr, source[currentInputLoc:])
	fmt.Fprintf(os.Stderr, "%*s", pos, "") // print pos spaces
	fmt.Fprint(os.Stderr, "^ ")
	fmt.Fprintf(os.Stderr, format+"\n", args...)
	os.Exit(1)
}

func failTok(tok *Token, format string, args ...any) {
	failAt(tok.loc, format, args...)
}

func main() {
	if len(os.Args) != 2 {
		fail("%s: invalid number of arguments", os.Args[0])
	}

	source = os.Args[1]
	tok := tokenize()

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
