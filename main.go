package main

import (
	"os"
)

func main() {
	if len(os.Args) != 3 {
		fail("%s: invalid number of arguments", os.Args[0])
	}

	// Tokenize and parse.
	tok := tokenize(os.Args[2])
	prog := parse(tok)

	// Traverse the AST to emit assembly.
	codegen(os.Args[1], prog)
}
