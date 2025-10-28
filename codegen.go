package main

import (
	"fmt"
	"os"
)

var outputFile *os.File
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
	println("  .globl %s", fname)
	println("  .text")
	println("%s:", fname)

	println("  push %%rbp")
	println("  mov %%rsp, %%rbp")
	println("  sub $%d, %%rsp", stackSize)
}

func (a X64) epilogue(fname string) {
	println(".L.return.%s:", fname)
	println("  mov %%rbp, %%rsp")
	println("  pop %%rbp")
	println("  ret")
}

func (a X64) push() {
	println("  push %%rax")
	depth++
}

func (a X64) pop(arg string) {
	println("  pop %s", arg)
	depth--
}

// Load a value from where %rax is pointing to.
func (a X64) load(ty *Type) {
	if ty.kind == TY_ARRAY {
		return
	}

	if ty.size == 1 {
		println("  movsbq (%%rax), %%rax")
	} else {
		println("  mov (%%rax), %%rax")
	}
}

// Store %rax to an address that the stack top is pointing to.
func (a X64) store(ty *Type) {
	a.pop("%rdi")

	if ty.size == 1 {
		println("  mov %%al, (%%rdi)")
	} else {
		println("  mov %%rax, (%%rdi)")
	}
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
func (a X64) genAddr(node *Node) {
	switch node.kind {
	case ND_VAR:
		if node.vara.isLocal {
			// Local variable
			println("  lea %d(%%rbp), %%rax", node.vara.offset)
		} else {
			// Global variable
			println("  lea %s(%%rip), %%rax", node.vara.name)
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
		println("  mov $%d, %%rax", node.val)
		return
	case ND_NEG:
		a.genExpr(node.lhs)
		println("  neg %%rax")
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
	case ND_STMT_EXPR:
		for n := node.body; n != nil; n = n.next {
			a.genStmt(n)
		}
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

		println("  mov $0, %%rax")
		println("  call %s", node.funcname)
		return
	}

	a.genExpr(node.rhs)
	a.push()
	a.genExpr(node.lhs)
	a.pop("%rdi")

	switch node.kind {
	case ND_ADD:
		println("  add %%rdi, %%rax")
		return
	case ND_SUB:
		println("  sub %%rdi, %%rax")
		return
	case ND_MUL:
		println("  imul %%rdi, %%rax")
		return
	case ND_DIV:
		println("  cqo")
		println("  idiv %%rdi")
		return
	case ND_EQ, ND_NE, ND_LT, ND_LE:
		println("  cmp %%rdi, %%rax")

		switch node.kind {
		case ND_EQ:
			println("  sete %%al")
		case ND_NE:
			println("  setne %%al")
		case ND_LT:
			println("  setl %%al")
		case ND_LE:
			println("  setle %%al")
		}

		println("  movzb %%al, %%rax")
		return
	}

	failTok(node.tok, "invalid expression")
}

func (a X64) genStmt(node *Node) {
	switch node.kind {
	case ND_IF:
		c := count()
		a.genExpr(node.cond)
		println("  cmp $0, %%rax")
		println("  je  .L.else.%d", c)
		a.genStmt(node.then)
		println("  jmp .L.end.%d", c)
		println(".L.else.%d:", c)
		if node.els != nil {
			a.genStmt(node.els)
		}
		println(".L.end.%d:", c)
		return
	case ND_FOR:
		c := count()
		if node.init != nil {
			a.genStmt(node.init)
		}
		println(".L.begin.%d:", c)
		if node.cond != nil {
			a.genExpr(node.cond)
			println("  cmp $0, %%rax")
			println("  je  .L.end.%d", c)
		}
		a.genStmt(node.then)
		if node.inc != nil {
			a.genExpr(node.inc)
		}
		println("  jmp .L.begin.%d", c)
		println(".L.end.%d:", c)
		return
	case ND_BLOCK:
		for n := node.body; n != nil; n = n.next {
			a.genStmt(n)
		}
		return
	case ND_RETURN:
		a.genExpr(node.lhs)
		println("  jmp .L.return.%s", currentGenFn.name)
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
				println("  mov %s, %d(%%rbp)", argReg8x[i], vara.offset)
			} else {
				println("  mov %s, %d(%%rbp)", argReg64x[i], vara.offset)
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
	println("  .globl %s", fname)
	println("  .text")
	println("%s:", fname)

	println("  addi sp, sp, -16")
	println("  sd ra, 8(sp)")
	println("  sd fp, 0(sp)")
	println("  mv fp, sp")

	println("  addi sp, sp, -%d", stackSize)
}

func (a RiscV) epilogue(fname string) {
	println(".L.return.%s:", fname)
	println("  mv sp, fp")
	println("  ld fp, 0(sp)")
	println("  ld ra, 8(sp)")
	println("  addi sp, sp, 16")
	println("  ret")
}

func (a RiscV) push() {
	println("  addi sp, sp, -8")
	println("  sd a0, 0(sp)")
	depth++
}

func (a RiscV) pop(arg string) {
	println("  ld %s, 0(sp)", arg)
	println("  addi sp, sp, 8")
	depth--
}

// Load a value from where %rax is pointing to.
func (a RiscV) load(ty *Type) {
	if ty.kind == TY_ARRAY {
		return
	}

	if ty.size == 1 {
		println("  lb a0, 0(a0)")
	} else {
		println("  ld a0, 0(a0)")
	}
}

// Store %rax to an address that the stack top is pointing to.
func (a RiscV) store(ty *Type) {
	a.pop("a1")

	if ty.size == 1 {
		println("  sb a0, 0(a1)")
	} else {
		println("  sd a0, 0(a1)")
	}
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
func (a RiscV) genAddr(node *Node) {
	switch node.kind {
	case ND_VAR:
		if node.vara.isLocal {
			println("  addi a0, fp, %d", node.vara.offset)
		} else {
			println("  la a0, %s", node.vara.name)
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
		println("  li a0, %d", node.val)
		return
	case ND_NEG:
		a.genExpr(node.lhs)
		println("  neg a0, a0")
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
	case ND_STMT_EXPR:
		for n := node.body; n != nil; n = n.next {
			a.genStmt(n)
		}
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

		println("  call %s", node.funcname)
		return
	}

	a.genExpr(node.rhs)
	a.push()
	a.genExpr(node.lhs)
	a.pop("a1")

	switch node.kind {
	case ND_ADD:
		println("  add a0, a0, a1")
		return
	case ND_SUB:
		println("  sub a0, a0, a1")
		return
	case ND_MUL:
		println("  mul a0, a0, a1")
		return
	case ND_DIV:
		println("  div a0, a0, a1")
		return
	case ND_EQ, ND_NE:
		println("  xor a0, a0, a1")

		if node.kind == ND_EQ {
			println("  seqz a0, a0")
		} else {
			println("  snez a0, a0")
		}

		return
	case ND_LT:
		println("  slt a0, a0, a1")
		return
	case ND_LE:
		println("  slt a0, a1, a0")
		println("  xori a0, a0, 1")
		return
	}

	failTok(node.tok, "invalid expression")
}

func (a RiscV) genStmt(node *Node) {
	switch node.kind {
	case ND_IF:
		c := count()
		a.genExpr(node.cond)
		println("  beqz a0, .L.else.%d", c)
		a.genStmt(node.then)
		println("  j .L.end.%d", c)
		println(".L.else.%d:", c)
		if node.els != nil {
			a.genStmt(node.els)
		}
		println(".L.end.%d:", c)
		return
	case ND_FOR:
		c := count()
		if node.init != nil {
			a.genStmt(node.init)
		}
		println(".L.begin.%d:", c)
		if node.cond != nil {
			a.genExpr(node.cond)
			println("  beqz a0, .L.end.%d", c)
		}
		a.genStmt(node.then)
		if node.inc != nil {
			a.genExpr(node.inc)
		}
		println("  j .L.begin.%d", c)
		println(".L.end.%d:", c)
		return
	case ND_BLOCK:
		for n := node.body; n != nil; n = n.next {
			a.genStmt(n)
		}
		return
	case ND_RETURN:
		a.genExpr(node.lhs)
		println("  j .L.return.%s", currentGenFn.name)
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
				println("  sb %s, %d(fp)", argRegR[i], vara.offset)
			} else {
				println("  sd %s, %d(fp)", argRegR[i], vara.offset)
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

		println("  .data")
		println("  .globl %s", vara.name)
		println("%s:", vara.name)

		if len(vara.initData) > 0 {
			for i := 0; i < vara.ty.size; i++ {
				println("  .byte %d", vara.initData[i])
			}
		} else {
			println("  .zero %d", vara.ty.size)
		}
	}
}

func codegen(arch string, prog *Obj, out *os.File) {
	outputFile = out
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

func println(format string, args ...any) {
	fmt.Fprintf(outputFile, format+"\n", args...)
}
