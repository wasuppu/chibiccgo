package main

import "fmt"

var depth int

type Arch interface {
	prologue()
	epilogue()
	genExpr(node *Node)
}

type X64 struct{}

func (a X64) prologue() {
	fmt.Printf("  .globl main\n")
	fmt.Printf("main:\n")
}

func (a X64) epilogue() {
	fmt.Printf("  ret\n")
}

func (a X64) push() {
	fmt.Printf("  push %%rax\n")
	depth++
}

func (a X64) pop(arg string) {
	fmt.Printf("  pop %s\n", arg)
	depth--
}

// Generate code for a given node.
func (a X64) genExpr(node *Node) {
	switch node.kind {
	case ND_NUM:
		fmt.Printf("  mov $%d, %%rax\n", node.val)
		return
	case ND_NEG:
		a.genExpr(node.lhs)
		fmt.Printf("  neg %%rax\n")
		return
	}

	a.genExpr(node.rhs)
	a.push()
	a.genExpr(node.lhs)
	a.pop("%rdi")

	switch node.kind {
	case ND_ADD:
		fmt.Printf("  add %%rdi, %%rax\n")
		return
	case ND_SUB:
		fmt.Printf("  sub %%rdi, %%rax\n")
		return
	case ND_MUL:
		fmt.Printf("  imul %%rdi, %%rax\n")
		return
	case ND_DIV:
		fmt.Printf("  cqo\n")
		fmt.Printf("  idiv %%rdi\n")
		return
	}

	fail("invalid expression")
}

type RiscV struct{}

func (a RiscV) prologue() {
	fmt.Printf("  .globl main\n")
	fmt.Printf("main:\n")
}

func (a RiscV) epilogue() {
	fmt.Printf("  ret\n")
}

func (a RiscV) push() {
	fmt.Printf("  addi sp, sp, -8\n")
	fmt.Printf("  sd a0, 0(sp)\n")
	depth++
}

func (a RiscV) pop(arg string) {
	fmt.Printf("  ld %s, 0(sp)\n", arg)
	fmt.Printf("  addi sp, sp, 8\n")
	depth--
}

// Generate code for a given node.
func (a RiscV) genExpr(node *Node) {
	switch node.kind {
	case ND_NUM:
		fmt.Printf("  li a0, %d\n", node.val)
		return
	case ND_NEG:
		a.genExpr(node.lhs)
		fmt.Printf("  neg a0, a0\n")
		return
	}

	a.genExpr(node.rhs)
	a.push()
	a.genExpr(node.lhs)
	a.pop("a1")

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
