package main

import (
	"fmt"
	"os"
)

var outputFile *os.File
var depth int
var currentGenFn *Obj

var argReg8x = []string{"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"}
var argReg16x = []string{"%di", "%si", "%dx", "%cx", "%r8w", "%r9w"}
var argReg32x = []string{"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"}
var argReg64x = []string{"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"}
var argRegR = []string{"a0", "a1", "a2", "a3", "a4", "a5"}

const (
	I8 = iota
	I16
	I32
	I64
)

// The table for x64 type casts
var i32i8 = "movsbl %al, %eax"
var i32i16 = "movswl %ax, %eax"
var i32i64 = "movsxd %eax, %rax"

var x64CastTable = [][]string{
	{"", "", "", i32i64},        // i8
	{i32i8, "", "", i32i64},     // i16
	{i32i8, i32i16, "", i32i64}, // i32
	{i32i8, i32i16, "", ""},     // i64
}

// The table for riscv type casts
var i64i8 = fmt.Sprint("slli a0, a0, 56\n", "  srai a0, a0, 56")
var i64i16 = fmt.Sprint("slli a0, a0, 48\n", "  srai a0, a0, 48")
var i64i32 = fmt.Sprint("slli a0, a0, 32\n", "  srai a0, a0, 32")
var riscvCastTable = [][]string{
	{"", "", "", ""},
	{i64i8, "", "", ""},
	{i64i8, i64i16, "", ""},
	{i64i8, i64i16, i64i32, ""},
}

func getTypeId(ty *Type) int {
	switch ty.kind {
	case TY_CHAR:
		return I8
	case TY_SHORT:
		return I16
	case TY_INT:
		return I32
	}
	return I64
}

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

func (a X64) prologue(fname string, stackSize int, isStatic bool) {
	if isStatic {
		println("  .local %s", fname)
	} else {
		println("  .globl %s", fname)
	}

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
	if ty.kind == TY_ARRAY || ty.kind == TY_STRUCT || ty.kind == TY_UNION {
		return
	}

	switch ty.size {
	case 1:
		println("  movsbl (%%rax), %%eax")
	case 2:
		println("  movswl (%%rax), %%eax")
	case 4:
		println("  movsxd (%%rax), %%rax")
	default:
		println("  mov (%%rax), %%rax")
	}
}

// Store %rax to an address that the stack top is pointing to.
func (a X64) store(ty *Type) {
	a.pop("%rdi")

	if ty.kind == TY_STRUCT || ty.kind == TY_UNION {
		for i := range ty.size {
			println("  mov %d(%%rax), %%r8b", i)
			println("  mov %%r8b, %d(%%rdi)", i)
		}
		return
	}

	switch ty.size {
	case 1:
		println("  mov %%al, (%%rdi)")
	case 2:
		println("  mov %%ax, (%%rdi)")
	case 4:
		println("  mov %%eax, (%%rdi)")
	default:
		println("  mov %%rax, (%%rdi)")
	}
}

func (a X64) storeGP(r, offset, sz int) {
	switch sz {
	case 1:
		println("  mov %s, %d(%%rbp)", argReg8x[r], offset)
		return
	case 2:
		println("  mov %s, %d(%%rbp)", argReg16x[r], offset)
		return
	case 4:
		println("  mov %s, %d(%%rbp)", argReg32x[r], offset)
		return
	case 8:
		println("  mov %s, %d(%%rbp)", argReg64x[r], offset)
		return
	}
	unreachable()
}

func (a X64) castType(from *Type, to *Type) {
	if to.kind == TY_VOID {
		return
	}

	if to.kind == TY_BOOL {
		a.cmpZero(from)
		println("  setne %%al")
		println("  movzx %%al, %%eax")
		return
	}

	t1 := getTypeId(from)
	t2 := getTypeId(to)
	if len(x64CastTable[t1][t2]) > 0 {
		println("  %s", x64CastTable[t1][t2])
	}
}

func (a X64) cmpZero(ty *Type) {
	if ty.isInteger() && ty.size <= 4 {
		println("  cmp $0, %%eax")
	} else {
		println("  cmp $0, %%rax")
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
	case ND_COMMA:
		a.genExpr(node.lhs)
		a.genAddr(node.rhs)
		return
	case ND_MEMBER:
		a.genAddr(node.lhs)
		println("  add $%d, %%rax", node.member.offset)
		return
	}

	failTok(node.tok, "not an lvalue")
}

// Generate code for a given node.
func (a X64) genExpr(node *Node) {
	println("  .loc 1 %d", node.tok.lineno)

	switch node.kind {
	case ND_NUM:
		println("  mov $%d, %%rax", node.val)
		return
	case ND_NEG:
		a.genExpr(node.lhs)
		println("  neg %%rax")
		return
	case ND_VAR, ND_MEMBER:
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
	case ND_COMMA:
		a.genExpr(node.lhs)
		a.genExpr(node.rhs)
		return
	case ND_CAST:
		a.genExpr(node.lhs)
		a.castType(node.lhs.ty, node.ty)
		return
	case ND_NOT:
		a.genExpr(node.lhs)
		println("  cmp $0, %%rax")
		println("  sete %%al")
		println("  movzx %%al, %%rax")
		return
	case ND_BITNOT:
		a.genExpr(node.lhs)
		println("  not %%rax")
		return
	case ND_LOGAND:
		c := count()
		a.genExpr(node.lhs)
		println("  cmp $0, %%rax")
		println("  je .L.false.%d", c)
		a.genExpr(node.rhs)
		println("  cmp $0, %%rax")
		println("  je .L.false.%d", c)
		println("  mov $1, %%rax")
		println("  jmp .L.end.%d", c)
		println(".L.false.%d:", c)
		println("  mov $0, %%rax")
		println(".L.end.%d:", c)
		return
	case ND_LOGOR:
		c := count()
		a.genExpr(node.lhs)
		println("  cmp $0, %%rax")
		println("  jne .L.true.%d", c)
		a.genExpr(node.rhs)
		println("  cmp $0, %%rax")
		println("  jne .L.true.%d", c)
		println("  mov $0, %%rax")
		println("  jmp .L.end.%d", c)
		println(".L.true.%d:", c)
		println("  mov $1, %%rax")
		println(".L.end.%d:", c)
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

	var ax, di string
	if node.lhs.ty.kind == TY_LONG || node.lhs.ty.base != nil {
		ax = "%rax"
		di = "%rdi"
	} else {
		ax = "%eax"
		di = "%edi"
	}

	switch node.kind {
	case ND_ADD:
		println("  add %s, %s", di, ax)
		return
	case ND_SUB:
		println("  sub %s, %s", di, ax)
		return
	case ND_MUL:
		println("  imul %s, %s", di, ax)
		return
	case ND_DIV, ND_MOD:
		if node.lhs.ty.size == 8 {
			println("  cqo")
		} else {
			println("  cdq")
		}
		println("  idiv %s", di)

		if node.kind == ND_MOD {
			println("  mov %%rdx, %%rax")
		}
		return
	case ND_BITAND:
		println("  and %%rdi, %%rax")
		return
	case ND_BITOR:
		println("  or %%rdi, %%rax")
		return
	case ND_BITXOR:
		println("  xor %%rdi, %%rax")
		return
	case ND_EQ, ND_NE, ND_LT, ND_LE:
		println("  cmp %s, %s", di, ax)

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
	println("  .loc 1 %d", node.tok.lineno)

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
		if !fn.isFunction || !fn.isDefinition {
			continue
		}

		// Prologue
		a.prologue(fn.name, fn.stackSize, fn.isStatic)
		currentGenFn = fn

		// Save passed-by-register arguments to the stack
		i := 0
		for vara := fn.params; vara != nil; vara = vara.next {
			a.storeGP(i, vara.offset, vara.ty.size)
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

func (a RiscV) prologue(fname string, stackSize int, isStatic bool) {
	if isStatic {
		println("  .local %s", fname)
	} else {
		println("  .globl %s", fname)
	}

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
	if ty.kind == TY_ARRAY || ty.kind == TY_STRUCT || ty.kind == TY_UNION {
		return
	}

	switch ty.size {
	case 1:
		println("  lb a0, 0(a0)")
	case 2:
		println("  lh a0, 0(a0)")
	case 4:
		println("  lw a0, 0(a0)")
	default:
		println("  ld a0, 0(a0)")
	}
}

// Store %rax to an address that the stack top is pointing to.
func (a RiscV) store(ty *Type) {
	a.pop("a1")

	if ty.kind == TY_STRUCT || ty.kind == TY_UNION {
		for i := range ty.size {
			println("  li t0, %d", i)
			println("  add t0, a0, t0")
			println("  lb t1, 0(t0)")

			println("  li t0, %d", i)
			println("  add t0, a1, t0")
			println("  sb t1, 0(t0)")
		}
		return
	}

	switch ty.size {
	case 1:
		println("  sb a0, 0(a1)")
	case 2:
		println("  sh a0, 0(a1)")
	case 4:
		println("  sw a0, 0(a1)")
	default:
		println("  sd a0, 0(a1)")
	}
}

func (a RiscV) storeGP(r, offset, sz int) {
	switch sz {
	case 1:
		println("  sb %s, %d(fp)", argRegR[r], offset)
		return
	case 2:
		println("  sh %s, %d(fp)", argRegR[r], offset)
		return
	case 4:
		println("  sw %s, %d(fp)", argRegR[r], offset)
		return
	case 8:
		println("  sd %s, %d(fp)", argRegR[r], offset)
		return
	}
	unreachable()
}

func (a RiscV) castType(from *Type, to *Type) {
	if to.kind == TY_VOID {
		return
	}

	if to.kind == TY_BOOL {
		println("  snez a0, a0")
		return
	}

	t1 := getTypeId(from)
	t2 := getTypeId(to)
	if len(riscvCastTable[t1][t2]) > 0 {
		println("  %s", riscvCastTable[t1][t2])
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
	case ND_COMMA:
		a.genExpr(node.lhs)
		a.genAddr(node.rhs)
		return
	case ND_MEMBER:
		a.genAddr(node.lhs)
		println("  li t0, %d", node.member.offset)
		println("  add a0, a0, t0")
		return
	}

	failTok(node.tok, "not an lvalue")
}

// Generate code for a given node.
func (a RiscV) genExpr(node *Node) {
	println("  .loc 1 %d", node.tok.lineno)

	switch node.kind {
	case ND_NUM:
		println("  li a0, %d", node.val)
		return
	case ND_NEG:
		a.genExpr(node.lhs)
		if node.ty.size <= 4 {
			println("  neg%s a0, a0", "w")
		} else {
			println("  neg a0, a0")
		}
		return
	case ND_VAR, ND_MEMBER:
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
	case ND_COMMA:
		a.genExpr(node.lhs)
		a.genExpr(node.rhs)
		return
	case ND_CAST:
		a.genExpr(node.lhs)
		a.castType(node.lhs.ty, node.ty)
		return
	case ND_NOT:
		a.genExpr(node.lhs)
		println("  seqz a0, a0")
		return
	case ND_BITNOT:
		a.genExpr(node.lhs)
		println("  not a0, a0")
		return
	case ND_LOGAND:
		c := count()
		a.genExpr(node.lhs)
		println("  beqz a0, .L.false.%d", c)
		a.genExpr(node.rhs)
		println("  beqz a0, .L.false.%d", c)
		println("  li a0, 1")
		println("  j .L.end.%d", c)
		println(".L.false.%d:", c)
		println("  li a0, 0")
		println(".L.end.%d:", c)
		return
	case ND_LOGOR:
		c := count()
		a.genExpr(node.lhs)
		println("  bnez a0, .L.true.%d", c)
		a.genExpr(node.rhs)
		println("  bnez a0, .L.true.%d", c)
		println("  li a0, 0")
		println("  j .L.end.%d", c)
		println(".L.true.%d:", c)
		println("  li a0, 1")
		println(".L.end.%d:", c)
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

	suffix := "w"
	if node.lhs.ty.kind == TY_LONG || node.lhs.ty.base != nil {
		suffix = ""
	}
	switch node.kind {
	case ND_ADD:
		println("  add%s a0, a0, a1", suffix)
		return
	case ND_SUB:
		println("  sub%s a0, a0, a1", suffix)
		return
	case ND_MUL:
		println("  mul%s a0, a0, a1", suffix)
		return
	case ND_DIV:
		println("  div%s a0, a0, a1", suffix)
		return
	case ND_MOD:
		println("  rem%s a0, a0, a1", suffix)
		return
	case ND_BITAND:
		println("  and a0, a0, a1")
		return
	case ND_BITOR:
		println("  or a0, a0, a1")
		return
	case ND_BITXOR:
		println("  xor a0, a0, a1")
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
	println("  .loc 1 %d", node.tok.lineno)

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
		if !fn.isFunction || !fn.isDefinition {
			continue
		}

		// Prologue
		a.prologue(fn.name, fn.stackSize, fn.isStatic)
		currentGenFn = fn

		// Save passed-by-register arguments to the stack
		i := 0
		for vara := fn.params; vara != nil; vara = vara.next {
			a.storeGP(i, vara.offset, vara.ty.size)
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
			offset = alignTo(offset, vara.ty.align)
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
