package main

import (
	"fmt"
)

var depth int

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

type Arch interface {
	prologue(stackSize int)
	epilogue()
	genStmt(node *Node)
}

type X64 struct{}

func (a X64) prologue(stackSize int) {
	fmt.Printf("  .globl main\n")
	fmt.Printf("main:\n")

	fmt.Printf("  push %%rbp\n")
	fmt.Printf("  mov %%rsp, %%rbp\n")
	fmt.Printf("  sub $%d, %%rsp\n", stackSize)
}

func (a X64) epilogue() {
	fmt.Printf(".L.return:\n")
	fmt.Printf("  mov %%rbp, %%rsp\n")
	fmt.Printf("  pop %%rbp\n")
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

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
func (a X64) genAddr(node *Node) {
	if node.kind == ND_VAR {
		fmt.Printf("  lea %d(%%rbp), %%rax\n", node.vara.offset)
		return
	}

	fail("not an lvalue")
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
	case ND_VAR:
		a.genAddr(node)
		fmt.Printf("  mov (%%rax), %%rax\n")
		return
	case ND_ASSIGN:
		a.genAddr(node.lhs)
		a.push()
		a.genExpr(node.rhs)
		a.pop("%rdi")
		fmt.Printf("  mov %%rax, (%%rdi)\n")
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
	case ND_EQ, ND_NE, ND_LT, ND_LE:
		fmt.Printf("  cmp %%rdi, %%rax\n")

		switch node.kind {
		case ND_EQ:
			fmt.Printf("  sete %%al\n")
		case ND_NE:
			fmt.Printf("  setne %%al\n")
		case ND_LT:
			fmt.Printf("  setl %%al\n")
		case ND_LE:
			fmt.Printf("  setle %%al\n")
		}

		fmt.Printf("  movzb %%al, %%rax\n")
		return
	}

	fail("invalid expression")
}

func (a X64) genStmt(node *Node) {
	switch node.kind {
	case ND_RETURN:
		a.genExpr(node.lhs)
		fmt.Printf("  jmp .L.return\n")
		return
	case ND_EXPR_STMT:
		a.genExpr(node.lhs)
		return
	}

	fail("invalid statement")
}

type RiscV struct{}

func (a RiscV) prologue(stackSize int) {
	fmt.Printf("  .globl main\n")
	fmt.Printf("main:\n")

	fmt.Printf("  addi sp, sp, -8\n")
	fmt.Printf("  sd fp, 0(sp)\n")
	fmt.Printf("  mv fp, sp\n")

	fmt.Printf("  addi sp, sp, -%d\n", stackSize)
}

func (a RiscV) epilogue() {
	fmt.Printf(".L.return:\n")
	fmt.Printf("  mv sp, fp\n")
	fmt.Printf("  ld fp, 0(sp)\n")
	fmt.Printf("  addi sp, sp, 8\n")
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

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
func (a RiscV) genAddr(node *Node) {
	if node.kind == ND_VAR {
		fmt.Printf("  addi a0, fp, %d\n", node.vara.offset)
		return
	}

	fail("not an lvalue")
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
	case ND_VAR:
		a.genAddr(node)
		fmt.Printf("  ld a0, 0(a0)\n")
		return
	case ND_ASSIGN:
		a.genAddr(node.lhs)
		a.push()
		a.genExpr(node.rhs)
		a.pop("a1")
		fmt.Printf("  sd a0, 0(a1)\n")
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
	case ND_EQ, ND_NE:
		fmt.Printf("  xor a0, a0, a1\n")

		if node.kind == ND_EQ {
			fmt.Printf("  seqz a0, a0\n")
		} else {
			fmt.Printf("  snez a0, a0\n")
		}

		return
	case ND_LT:
		fmt.Printf("  slt a0, a0, a1\n")
		return
	case ND_LE:
		fmt.Printf("  slt a0, a1, a0\n")
		fmt.Printf("  xori a0, a0, 1\n")
		return
	}

	fail("invalid expression")
}

func (a RiscV) genStmt(node *Node) {
	switch node.kind {
	case ND_RETURN:
		a.genExpr(node.lhs)
		fmt.Printf("  j .L.return\n")
		return
	case ND_EXPR_STMT:
		a.genExpr(node.lhs)
		return
	}

	fail("invalid statement")
}

func codegen(arch string, prog *Function) {
	assignLVarOffsets(prog)

	target := chooseArch(arch)
	target.prologue(prog.stackSize)

	for n := prog.body; n != nil; n = n.next {
		target.genStmt(n)
		assert(depth == 0)
	}

	target.epilogue()
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
func alignTo(n, align int) int {
	return (n + align - 1) / align * align
}

// Assign offsets to local variables.
func assignLVarOffsets(prog *Function) {
	offset := 0
	for vara := prog.locals; vara != nil; vara = vara.next {
		offset += 8
		vara.offset = -offset
	}
	prog.stackSize = alignTo(offset, 16)
}
