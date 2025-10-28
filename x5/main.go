package main

import (
	"fmt"
	"os"
)

var source string
var currentInputLoc int

func main() {
	if len(os.Args) != 2 {
		fail("%s: invalid number of arguments", os.Args[0])
	}

	// Tokenize and parse.
	source = os.Args[1]
	tok := tokenize()
	node := expr(&tok, tok)

	if tok.kind != TK_EOF {
		failTok(tok, "extra token")
	}

	fmt.Printf("  .globl main\n")
	fmt.Printf("main:\n")

	// Traverse the AST to emit assembly.
	genExpr(node)

	fmt.Printf("  ret\n")

	assert(depth == 0)
}
