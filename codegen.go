package main

import (
	"fmt"
)

var depth int
var currentGenFn *Obj

var argReg8x = []string{"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"}
var argReg64x = []string{"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"}
var argRegR = []string{"a0", "a1", "a2", "a3", "a4", "a5"}

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
	emitText(prog *Obj)
}

type X64 struct{}

func (a X64) prologue(fname string, stackSize int) {
	fmt.Printf("  .globl %s\n", fname)
	fmt.Printf("  .text\n")
	fmt.Printf("%s:\n", fname)

	fmt.Printf("  push %%rbp\n")
	fmt.Printf("  mov %%rsp, %%rbp\n")
	fmt.Printf("  sub $%d, %%rsp\n", stackSize)
}

func (a X64) epilogue(fname string) {
	fmt.Printf(".L.return.%s:\n", fname)
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

// Load a value from where %rax is pointing to.
func (a X64) load(ty *Type) {
	if ty.kind == TY_ARRAY {
		return
	}

	if ty.size == 1 {
		fmt.Printf("  movsbq (%%rax), %%rax\n")
	} else {
		fmt.Printf("  mov (%%rax), %%rax\n")
	}
}

// Store %rax to an address that the stack top is pointing to.
func (a X64) store(ty *Type) {
	a.pop("%rdi")

	if ty.size == 1 {
		fmt.Printf("  mov %%al, (%%rdi)\n")
	} else {
		fmt.Printf("  mov %%rax, (%%rdi)\n")
	}
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
func (a X64) genAddr(node *Node) {
	switch node.kind {
	case ND_VAR:
		if node.vara.isLocal {
			// Local variable
			fmt.Printf("  lea %d(%%rbp), %%rax\n", node.vara.offset)
		} else {
			// Global variable
			fmt.Printf("  lea %s(%%rip), %%rax\n", node.vara.name)
		}
		return
	case ND_DEREF:
		a.genExpr(node.lhs)
		return
	}

	failTok(node.tok, "not an lvalue")
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
		a.load(node.ty)
		return
	case ND_DEREF:
		a.genExpr(node.lhs)
		a.load(node.ty)
		return
	case ND_ADDR:
		a.genAddr(node.lhs)
		return
	case ND_ASSIGN:
		a.genAddr(node.lhs)
		a.push()
		a.genExpr(node.rhs)
		a.store(node.ty)
		return
	case ND_FUNCALL:
		nargs := 0
		for arg := node.args; arg != nil; arg = arg.next {
			a.genExpr(arg)
			a.push()
			nargs++
		}

		for i := nargs - 1; i >= 0; i-- {
			a.pop(argReg64x[i])
		}

		fmt.Printf("  mov $0, %%rax\n")
		fmt.Printf("  call %s\n", node.funcname)
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

	failTok(node.tok, "invalid expression")
}

func (a X64) genStmt(node *Node) {
	switch node.kind {
	case ND_IF:
		c := count()
		a.genExpr(node.cond)
		fmt.Printf("  cmp $0, %%rax\n")
		fmt.Printf("  je  .L.else.%d\n", c)
		a.genStmt(node.then)
		fmt.Printf("  jmp .L.end.%d\n", c)
		fmt.Printf(".L.else.%d:\n", c)
		if node.els != nil {
			a.genStmt(node.els)
		}
		fmt.Printf(".L.end.%d:\n", c)
		return
	case ND_FOR:
		c := count()
		if node.init != nil {
			a.genStmt(node.init)
		}
		fmt.Printf(".L.begin.%d:\n", c)
		if node.cond != nil {
			a.genExpr(node.cond)
			fmt.Printf("  cmp $0, %%rax\n")
			fmt.Printf("  je  .L.end.%d\n", c)
		}
		a.genStmt(node.then)
		if node.inc != nil {
			a.genExpr(node.inc)
		}
		fmt.Printf("  jmp .L.begin.%d\n", c)
		fmt.Printf(".L.end.%d:\n", c)
		return
	case ND_BLOCK:
		for n := node.body; n != nil; n = n.next {
			a.genStmt(n)
		}
		return
	case ND_RETURN:
		a.genExpr(node.lhs)
		fmt.Printf("  jmp .L.return.%s\n", currentGenFn.name)
		return
	case ND_EXPR_STMT:
		a.genExpr(node.lhs)
		return
	}

	failTok(node.tok, "invalid statement")
}

func (a X64) emitText(prog *Obj) {
	for fn := prog; fn != nil; fn = fn.next {
		if !fn.isFunction {
			continue
		}

		// Prologue
		a.prologue(fn.name, fn.stackSize)
		currentGenFn = fn

		// Save passed-by-register arguments to the stack
		i := 0
		for vara := fn.params; vara != nil; vara = vara.next {
			if vara.ty.size == 1 {
				fmt.Printf("  mov %s, %d(%%rbp)\n", argReg8x[i], vara.offset)
			} else {
				fmt.Printf("  mov %s, %d(%%rbp)\n", argReg64x[i], vara.offset)
			}
			i++
		}

		// Emit code
		a.genStmt(fn.body)
		assert(depth == 0)

		// Epilogue
		a.epilogue(fn.name)
	}
}

type RiscV struct{}

func (a RiscV) prologue(fname string, stackSize int) {
	fmt.Printf("  .globl %s\n", fname)
	fmt.Printf("  .text\n")
	fmt.Printf("%s:\n", fname)

	fmt.Printf("  addi sp, sp, -16\n")
	fmt.Printf("  sd ra, 8(sp)\n")
	fmt.Printf("  sd fp, 0(sp)\n")
	fmt.Printf("  mv fp, sp\n")

	fmt.Printf("  addi sp, sp, -%d\n", stackSize)
}

func (a RiscV) epilogue(fname string) {
	fmt.Printf(".L.return.%s:\n", fname)
	fmt.Printf("  mv sp, fp\n")
	fmt.Printf("  ld fp, 0(sp)\n")
	fmt.Printf("  ld ra, 8(sp)\n")
	fmt.Printf("  addi sp, sp, 16\n")
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

// Load a value from where %rax is pointing to.
func (a RiscV) load(ty *Type) {
	if ty.kind == TY_ARRAY {
		return
	}

	if ty.size == 1 {
		fmt.Printf("  lb a0, 0(a0)\n")
	} else {
		fmt.Printf("  ld a0, 0(a0)\n")
	}
}

// Store %rax to an address that the stack top is pointing to.
func (a RiscV) store(ty *Type) {
	a.pop("a1")

	if ty.size == 1 {
		fmt.Printf("  sb a0, 0(a1)\n")
	} else {
		fmt.Printf("  sd a0, 0(a1)\n")
	}
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
func (a RiscV) genAddr(node *Node) {
	switch node.kind {
	case ND_VAR:
		if node.vara.isLocal {
			fmt.Printf("  addi a0, fp, %d\n", node.vara.offset)
		} else {
			fmt.Printf("  la a0, %s\n", node.vara.name)
		}
		return
	case ND_DEREF:
		a.genExpr(node.lhs)
		return
	}

	failTok(node.tok, "not an lvalue")
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
		a.load(node.ty)
		return
	case ND_DEREF:
		a.genExpr(node.lhs)
		a.load(node.ty)
		return
	case ND_ADDR:
		a.genAddr(node.lhs)
		return
	case ND_ASSIGN:
		a.genAddr(node.lhs)
		a.push()
		a.genExpr(node.rhs)
		a.store(node.ty)
		return
	case ND_FUNCALL:
		nargs := 0
		for arg := node.args; arg != nil; arg = arg.next {
			a.genExpr(arg)
			a.push()
			nargs++
		}

		for i := nargs - 1; i >= 0; i-- {
			a.pop(argRegR[i])
		}

		fmt.Printf("  call %s\n", node.funcname)
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

	failTok(node.tok, "invalid expression")
}

func (a RiscV) genStmt(node *Node) {
	switch node.kind {
	case ND_IF:
		c := count()
		a.genExpr(node.cond)
		fmt.Printf("  beqz a0, .L.else.%d\n", c)
		a.genStmt(node.then)
		fmt.Printf("  j .L.end.%d\n", c)
		fmt.Printf(".L.else.%d:\n", c)
		if node.els != nil {
			a.genStmt(node.els)
		}
		fmt.Printf(".L.end.%d:\n", c)
		return
	case ND_FOR:
		c := count()
		if node.init != nil {
			a.genStmt(node.init)
		}
		fmt.Printf(".L.begin.%d:\n", c)
		if node.cond != nil {
			a.genExpr(node.cond)
			fmt.Printf("  beqz a0, .L.end.%d\n", c)
		}
		a.genStmt(node.then)
		if node.inc != nil {
			a.genExpr(node.inc)
		}
		fmt.Printf("  j .L.begin.%d\n", c)
		fmt.Printf(".L.end.%d:\n", c)
		return
	case ND_BLOCK:
		for n := node.body; n != nil; n = n.next {
			a.genStmt(n)
		}
		return
	case ND_RETURN:
		a.genExpr(node.lhs)
		fmt.Printf("  j .L.return.%s\n", currentGenFn.name)
		return
	case ND_EXPR_STMT:
		a.genExpr(node.lhs)
		return
	}

	failTok(node.tok, "invalid statement")
}

func (a RiscV) emitText(prog *Obj) {
	for fn := prog; fn != nil; fn = fn.next {
		if !fn.isFunction {
			continue
		}

		// Prologue
		a.prologue(fn.name, fn.stackSize)
		currentGenFn = fn

		// Save passed-by-register arguments to the stack
		i := 0
		for vara := fn.params; vara != nil; vara = vara.next {
			if vara.ty.size == 1 {
				fmt.Printf("  sb %s, %d(fp)\n", argRegR[i], vara.offset)
			} else {
				fmt.Printf("  sd %s, %d(fp)\n", argRegR[i], vara.offset)
			}
			i++
		}

		// Emit code
		a.genStmt(fn.body)
		assert(depth == 0)

		// Epilogue
		a.epilogue(fn.name)
	}
}

func emitData(prog *Obj) {
	for vara := prog; vara != nil; vara = vara.next {
		if vara.isFunction {
			continue
		}

		fmt.Printf("  .data\n")
		fmt.Printf("  .globl %s\n", vara.name)
		fmt.Printf("%s:\n", vara.name)

		if len(vara.initData) > 0 {
			for i := 0; i < vara.ty.size; i++ {
				fmt.Printf("  .byte %d\n", vara.initData[i])
			}
		} else {
			fmt.Printf("  .zero %d\n", vara.ty.size)
		}
	}
}

func codegen(arch string, prog *Obj) {
	assignLVarOffsets(prog)
	emitData(prog)

	target := chooseArch(arch)
	target.emitText(prog)
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
func alignTo(n, align int) int {
	return (n + align - 1) / align * align
}

// Assign offsets to local variables.
func assignLVarOffsets(prog *Obj) {
	for fn := prog; fn != nil; fn = fn.next {
		if !fn.isFunction {
			continue
		}
		offset := 0
		for vara := fn.locals; vara != nil; vara = vara.next {
			offset += vara.ty.size
			vara.offset = -offset
		}
		fn.stackSize = alignTo(offset, 16)
	}
}

var I = 1

func count() int {
	t := I
	I++
	return t
}
