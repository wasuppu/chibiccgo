package main

import (
	"os"
)

var source string
var currentInputLoc int

func chooseArch(arch string) Arch {
	var target Arch
	switch arch {
	case "x64":
		target = &X64{}
	case "riscv":
		target = &RiscV{}
	default:
		fail("unsupported architecture: %s", arch)
	}
	return target
}

func main() {
	if len(os.Args) != 3 {
		fail("%s: invalid number of arguments", os.Args[0])
	}

	// Tokenize and parse.
	target := chooseArch(os.Args[1])
	source = os.Args[2]
	tok := tokenize()
	node := expr(&tok, tok)

	if tok.kind != TK_EOF {
		failTok(tok, "extra token")
	}

	target.prologue()
	target.genExpr(node)
	target.epilogue()

	assert(depth == 0)
}
