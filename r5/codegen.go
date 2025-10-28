package main

import "fmt"

var depth int

func push() {
	fmt.Printf("  addi sp, sp, -8\n")
	fmt.Printf("  sd a0, 0(sp)\n")
	depth++
}

func pop(arg string) {
	fmt.Printf("  ld %s, 0(sp)\n", arg)
	fmt.Printf("  addi sp, sp, 8\n")
	depth--
}

// Generate code for a given node.
func genExpr(node *Node) {
	if node.kind == ND_NUM {
		fmt.Printf("  li a0, %d\n", node.val)
		return
	}

	genExpr(node.rhs)
	push()
	genExpr(node.lhs)
	pop("a1")

	switch node.kind {
	case ND_ADD:
		fmt.Printf("  add a0, a0, a1\n")
		return
	case ND_SUB:
		fmt.Printf("  sub a0, a0, a1\n")
		return
	case ND_MUL:
		fmt.Printf("  mul a0, a0, a1\n")
		return
	case ND_DIV:
		fmt.Printf("  div a0, a0, a1\n")
		return
	}

	fail("invalid expression")
}
