package main

import (
	"fmt"
	"math"
	"os"
)

var ArchName string
var outputFile *os.File
var depth int
var currentGenFn *Obj

var argReg8x = []string{"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"}
var argReg16x = []string{"%di", "%si", "%dx", "%cx", "%r8w", "%r9w"}
var argReg32x = []string{"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"}
var argReg64x = []string{"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"}
var argRegR = []string{"a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"}

const (
	I8 = iota
	I16
	I32
	I64
	U8
	U16
	U32
	U64
	F32
	F64
)

// The table for x64 type casts
var i32i8x = "movsbl %al, %eax"
var i32u8x = "movzbl %al, %eax"
var i32i16x = "movswl %ax, %eax"
var i32u16x = "movzwl %ax, %eax"
var i32f32x = "cvtsi2ssl %eax, %xmm0"
var i32i64x = "movsxd %eax, %rax"
var i32f64x = "cvtsi2sdl %eax, %xmm0"

var u32f32x = "mov %eax, %eax; cvtsi2ssq %rax, %xmm0"
var u32i64x = "mov %eax, %eax"
var u32f64x = "mov %eax, %eax; cvtsi2sdq %rax, %xmm0"

var i64f32x = "cvtsi2ssq %rax, %xmm0"
var i64f64x = "cvtsi2sdq %rax, %xmm0"

var u64f32x = "cvtsi2ssq %rax, %xmm0"
var u64f64x = "test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %rax,%xmm0; jmp 2f; " +
	"1: mov %rax,%rdi; and $1,%eax; pxor %xmm0,%xmm0; shr %rdi; " +
	"or %rax,%rdi; cvtsi2sd %rdi,%xmm0; addsd %xmm0,%xmm0; 2:"

var f32i8x = "cvttss2sil %xmm0, %eax; movsbl %al, %eax"
var f32u8x = "cvttss2sil %xmm0, %eax; movzbl %al, %eax"
var f32i16x = "cvttss2sil %xmm0, %eax; movswl %ax, %eax"
var f32u16x = "cvttss2sil %xmm0, %eax; movzwl %ax, %eax"
var f32i32x = "cvttss2sil %xmm0, %eax"
var f32u32x = "cvttss2siq %xmm0, %rax"
var f32i64x = "cvttss2siq %xmm0, %rax"
var f32u64x = "cvttss2siq %xmm0, %rax"
var f32f64x = "cvtss2sd %xmm0, %xmm0"

var f64i8x = "cvttsd2sil %xmm0, %eax; movsbl %al, %eax"
var f64u8x = "cvttsd2sil %xmm0, %eax; movzbl %al, %eax"
var f64i16x = "cvttsd2sil %xmm0, %eax; movswl %ax, %eax"
var f64u16x = "cvttsd2sil %xmm0, %eax; movzwl %ax, %eax"
var f64i32x = "cvttsd2sil %xmm0, %eax"
var f64u32x = "cvttsd2siq %xmm0, %rax"
var f64f32x = "cvtsd2ss %xmm0, %xmm0"
var f64i64x = "cvttsd2siq %xmm0, %rax"
var f64u64x = "cvttsd2siq %xmm0, %rax"

var x64CastTable = [][]string{
	// i8   i16   i32   i64   u8   u16   u32   u64   f32   f64
	{"", "", "", i32i64x, i32u8x, i32u16x, "", i32i64x, i32f32x, i32f64x},               // i8
	{i32i8x, "", "", i32i64x, i32u8x, i32u16x, "", i32i64x, i32f32x, i32f64x},           // i16
	{i32i8x, i32i16x, "", i32i64x, i32u8x, i32u16x, "", i32i64x, i32f32x, i32f64x},      // i32
	{i32i8x, i32i16x, "", "", i32u8x, i32u16x, "", "", i64f32x, i64f64x},                // i64
	{i32i8x, "", "", i32i64x, "", "", "", i32i64x, i32f32x, i32f64x},                    // u8
	{i32i8x, i32i16x, "", i32i64x, i32u8x, "", "", i32i64x, i32f32x, i32f64x},           // u16
	{i32i8x, i32i16x, "", u32i64x, i32u8x, i32u16x, "", u32i64x, u32f32x, u32f64x},      // u32
	{i32i8x, i32i16x, "", "", i32u8x, i32u16x, "", "", u64f32x, u64f64x},                // u64
	{f32i8x, f32i16x, f32i32x, f32i64x, f32u8x, f32u16x, f32u32x, f32u64x, "", f32f64x}, // f32
	{f64i8x, f64i16x, f64i32x, f64i64x, f64u8x, f64u16x, f64u32x, f64u64x, f64f32x, ""}, // f64
}

// The table for riscv type casts
var i32f32r = "fcvt.s.w fa0, a0"
var i32f64r = "fcvt.d.w fa0, a0"

var i64i8r = fmt.Sprint("slli a0, a0, 56\n", "  srai a0, a0, 56")
var i64u8r = fmt.Sprint("slli a0, a0, 56\n", "  srli a0, a0, 56")
var i64i16r = fmt.Sprint("slli a0, a0, 48\n", "  srai a0, a0, 48")
var i64u16r = fmt.Sprint("slli a0, a0, 48\n", "  srli a0, a0, 48")
var i64i32r = fmt.Sprint("slli a0, a0, 32\n", "  srai a0, a0, 32")
var i64u32r = fmt.Sprint("slli a0, a0, 32\n", "  srli a0, a0, 32")

var i64f32r = "fcvt.s.l fa0, a0"
var i64f64r = "fcvt.d.l fa0, a0"

var u32f32r = "fcvt.s.wu fa0, a0"
var u32f64r = "fcvt.d.wu fa0, a0"

var u32i64r = fmt.Sprint("slli a0, a0, 32\n", "  srli a0, a0, 32")

var u64f32r = "fcvt.s.lu fa0, a0"
var u64f64r = "fcvt.d.lu fa0, a0"

var f32i8r = fmt.Sprint("fcvt.w.s a0, fa0, rtz\n", "  slli a0, a0, 56\n", "  srai a0, a0, 56")
var f32i16r = fmt.Sprint("fcvt.w.s a0, fa0, rtz\n", "  slli a0, a0, 48\n", "  srai a0, a0, 48")
var f32i32r = fmt.Sprint("fcvt.w.s a0, fa0, rtz\n", "  slli a0, a0, 32\n", "  srai a0, a0, 32")
var f32i64r = "fcvt.l.s a0, fa0, rtz"

var f32u8r = fmt.Sprint("fcvt.wu.s a0, fa0, rtz\n", "  slli a0, a0, 56\n", "  srli a0, a0, 56")
var f32u16r = fmt.Sprint("fcvt.wu.s a0, fa0, rtz\n", "  slli a0, a0, 48\n", "  srli a0, a0, 48\n")
var f32u32r = fmt.Sprint("fcvt.wu.s a0, fa0, rtz\n", "  slli a0, a0, 32\n", "  srai a0, a0, 32")
var f32u64r = "fcvt.lu.s a0, fa0, rtz"

var f32f64r = "fcvt.d.s fa0, fa0"

var f64i8r = fmt.Sprint("fcvt.w.d a0, fa0, rtz\n", "  slli a0, a0, 56\n", "  srai a0, a0, 56")
var f64i16r = fmt.Sprint("fcvt.w.d a0, fa0, rtz\n", "  slli a0, a0, 48\n", "  srai a0, a0, 48")
var f64i32r = fmt.Sprint("fcvt.w.d a0, fa0, rtz\n", "  slli a0, a0, 32\n", "  srai a0, a0, 32")
var f64i64r = "fcvt.l.d a0, fa0, rtz"

var f64u8r = fmt.Sprint("fcvt.wu.d a0, fa0, rtz\n", "  slli a0, a0, 56\n", "  srli a0, a0, 56")
var f64u16r = fmt.Sprint("fcvt.wu.d a0, fa0, rtz\n", "  slli a0, a0, 48\n", "  srli a0, a0, 48")
var f64u32r = fmt.Sprint("fcvt.wu.d a0, fa0, rtz\n", "  slli a0, a0, 32\n", "  srai a0, a0, 32")
var f64u64r = "fcvt.lu.d a0, fa0, rtz"

var f64f32r = "fcvt.s.d fa0, fa0"

var riscvCastTable = [][]string{
	// i8   i16   i32   i64   u8   u16   u32   u64   f32   f64
	{"", "", "", "", i64u8r, i64u16r, i64u32r, "", i32f32r, i32f64r},                    // i8
	{i64i8r, "", "", "", i64u8r, i64u16r, i64u32r, "", i32f32r, i32f64r},                // i16
	{i64i8r, i64i16r, "", "", i64u8r, i64u16r, i64u32r, "", i32f32r, i32f64r},           // i32
	{i64i8r, i64i16r, i64i32r, "", i64u8r, i64u16r, i64u32r, "", i64f32r, i64f64r},      // i64
	{i64i8r, "", "", "", "", "", "", "", u32f32r, u32f64r},                              // u8
	{i64i8r, i64i16r, "", "", i64u8r, "", "", "", u32f32r, u32f64r},                     // u16
	{i64i8r, i64i16r, i64i32r, u32i64r, i64u8r, i64u16r, "", u32i64r, u32f32r, u32f64r}, // u32
	{i64i8r, i64i16r, i64i32r, "", i64u8r, i64u16r, i64u32r, "", u64f32r, u64f64r},      // u64
	{f32i8r, f32i16r, f32i32r, f32i64r, f32u8r, f32u16r, f32u32r, f32u64r, "", f32f64r}, // f32
	{f64i8r, f64i16r, f64i32r, f64i64r, f64u8r, f64u16r, f64u32r, f64u64r, f64f32r, ""}, // f64
}

func getTypeId(ty *Type) int {
	switch ty.kind {
	case TY_CHAR:
		if ty.isUnsigned {
			return U8
		} else {
			return I8
		}
	case TY_SHORT:
		if ty.isUnsigned {
			return U16
		} else {
			return I16
		}
	case TY_INT:
		if ty.isUnsigned {
			return U32
		} else {
			return I32
		}
	case TY_LONG:
		if ty.isUnsigned {
			return U64
		} else {
			return I64
		}
	case TY_FLOAT:
		return F32
	case TY_DOUBLE:
		return F64
	}
	return U64
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
	ArchName = arch
	return target
}

type Arch interface {
	emitText(prog *Obj)
	emitData(prog *Obj)
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

func (a X64) pushf() {
	println("  sub $8, %%rsp")
	println("  movsd %%xmm0, (%%rsp)")
	depth++
}

func (a X64) popf(arg string) {
	println("  movsd (%%rsp), %s", arg)
	println("  add $8, %%rsp")
	depth--
}

// Load a value from where %rax is pointing to.
func (a X64) load(ty *Type) {
	switch ty.kind {
	case TY_ARRAY, TY_STRUCT, TY_UNION:
		return
	case TY_FLOAT:
		println("  movss (%%rax), %%xmm0")
		return
	case TY_DOUBLE:
		println("  movsd (%%rax), %%xmm0")
		return
	}

	var insn string
	if ty.isUnsigned {
		insn = "movz"
	} else {
		insn = "movs"
	}

	switch ty.size {
	case 1:
		println("  %sbl (%%rax), %%eax", insn)
	case 2:
		println("  %swl (%%rax), %%eax", insn)
	case 4:
		println("  movsxd (%%rax), %%rax")
	default:
		println("  mov (%%rax), %%rax")
	}
}

// Store %rax to an address that the stack top is pointing to.
func (a X64) store(ty *Type) {
	a.pop("%rdi")

	switch ty.kind {
	case TY_STRUCT, TY_UNION:
		for i := range ty.size {
			println("  mov %d(%%rax), %%r8b", i)
			println("  mov %%r8b, %d(%%rdi)", i)
		}
		return
	case TY_FLOAT:
		println("  movss %%xmm0, (%%rdi)")
		return
	case TY_DOUBLE:
		println("  movsd %%xmm0, (%%rdi)")
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
	case ND_NULL_EXPR:
		return
	case ND_NUM:
		switch node.ty.kind {
		case TY_FLOAT:
			f32 := float32(node.fval)
			u32 := math.Float32bits(f32)
			println("  mov $%d, %%eax  # float %f", u32, node.fval)
			println("  movq %%rax, %%xmm0")
			return
		case TY_DOUBLE:
			f64 := node.fval
			u64 := math.Float64bits(f64)
			println("  mov $%d, %%rax  # double %f", u64, node.fval)
			println("  movq %%rax, %%xmm0")
			return
		}

		println("  mov $%d, %%rax", node.val)
		return
	case ND_NEG:
		a.genExpr(node.lhs)

		switch node.ty.kind {
		case TY_FLOAT:
			println("  mov $1, %%rax")
			println("  shl $31, %%rax")
			println("  movq %%rax, %%xmm1")
			println("  xorps %%xmm1, %%xmm0")
			return
		case TY_DOUBLE:
			println("  mov $1, %%rax")
			println("  shl $63, %%rax")
			println("  movq %%rax, %%xmm1")
			println("  xorpd %%xmm1, %%xmm0")
			return
		}

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
	case ND_MEMZERO:
		// `rep stosb` is equivalent to `memset(%rdi, %al, %rcx)`.
		println("  mov $%d, %%rcx", node.vara.ty.size)
		println("  lea %d(%%rbp), %%rdi", node.vara.offset)
		println("  mov $0, %%al")
		println("  rep stosb")
		return
	case ND_COND:
		c := count()
		a.genExpr(node.cond)
		println("  cmp $0, %%rax")
		println("  je .L.else.%d", c)
		a.genExpr(node.then)
		println("  jmp .L.end.%d", c)
		println(".L.else.%d:", c)
		a.genExpr(node.els)
		println(".L.end.%d:", c)
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

		if depth%2 == 0 {
			println("  call %s", node.funcname)
		} else {
			println("  sub $8, %%rsp")
			println("  call %s", node.funcname)
			println("  add $8, %%rsp")
		}

		// It looks like the most significant 48 or 56 bits in RAX may
		// contain garbage if a function return type is short or bool/char,
		// respectively. We clear the upper bits here.
		switch node.ty.kind {
		case TY_BOOL:
			println("  movzx %%al, %%eax")
			return
		case TY_CHAR:
			if node.ty.isUnsigned {
				println("  movzbl %%al, %%eax")
			} else {
				println("  movsbl %%al, %%eax")
			}
			return
		case TY_SHORT:
			if node.ty.isUnsigned {
				println("  movzwl %%ax, %%eax")
			} else {
				println("  movswl %%ax, %%eax")
			}
			return
		}
		return
	}

	if node.lhs.ty.isFlonum() {
		a.genExpr(node.rhs)
		a.pushf()
		a.genExpr(node.lhs)
		a.popf("%xmm1")

		var sz string
		if node.lhs.ty.kind == TY_FLOAT {
			sz = "ss"
		} else {
			sz = "sd"
		}

		switch node.kind {
		case ND_ADD:
			println("  add%s %%xmm1, %%xmm0", sz)
			return
		case ND_SUB:
			println("  sub%s %%xmm1, %%xmm0", sz)
			return
		case ND_MUL:
			println("  mul%s %%xmm1, %%xmm0", sz)
			return
		case ND_DIV:
			println("  div%s %%xmm1, %%xmm0", sz)
			return
		case ND_EQ, ND_NE, ND_LT, ND_LE:
			println("  ucomi%s %%xmm0, %%xmm1", sz)

			switch node.kind {
			case ND_EQ:
				println("  sete %%al")
				println("  setnp %%dl")
				println("  and %%dl, %%al")
			case ND_NE:
				println("  setne %%al")
				println("  setp %%dl")
				println("  or %%dl, %%al")
			case ND_LT:
				println("  seta %%al")
			default:
				println("  setae %%al")
			}

			println("  and $1, %%al")
			println("  movzb %%al, %%rax")
			return
		}

		failTok(node.tok, "invalid expression")
	}

	a.genExpr(node.rhs)
	a.push()
	a.genExpr(node.lhs)
	a.pop("%rdi")

	var ax, di, dx string
	if node.lhs.ty.kind == TY_LONG || node.lhs.ty.base != nil {
		ax = "%rax"
		di = "%rdi"
		dx = "%rdx"
	} else {
		ax = "%eax"
		di = "%edi"
		dx = "%edx"
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
		if node.ty.isUnsigned {
			println("  mov $0, %s", dx)
			println("  div %s", di)
		} else {
			if node.lhs.ty.size == 8 {
				println("  cqo")
			} else {
				println("  cdq")
			}
			println("  idiv %s", di)
		}

		if node.kind == ND_MOD {
			println("  mov %%rdx, %%rax")
		}
		return
	case ND_BITAND:
		println("  and %s, %s", di, ax)
		return
	case ND_BITOR:
		println("  or %s, %s", di, ax)
		return
	case ND_BITXOR:
		println("  xor %s, %s", di, ax)
		return
	case ND_EQ, ND_NE, ND_LT, ND_LE:
		println("  cmp %s, %s", di, ax)

		switch node.kind {
		case ND_EQ:
			println("  sete %%al")
		case ND_NE:
			println("  setne %%al")
		case ND_LT:
			if node.lhs.ty.isUnsigned {
				println("  setb %%al")
			} else {
				println("  setl %%al")
			}
		case ND_LE:
			if node.lhs.ty.isUnsigned {
				println("  setbe %%al")
			} else {
				println("  setle %%al")
			}
		}

		println("  movzb %%al, %%rax")
		return
	case ND_SHL:
		println("  mov %%rdi, %%rcx")
		println("  shl %%cl, %s", ax)
		return
	case ND_SHR:
		println("  mov %%rdi, %%rcx")
		if node.lhs.ty.isUnsigned {
			println("  shr %%cl, %s", ax)
		} else {
			println("  sar %%cl, %s", ax)
		}
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
			println("  je %s", node.brkLabel)
		}
		a.genStmt(node.then)
		println("%s:", node.contLabel)
		if node.inc != nil {
			a.genExpr(node.inc)
		}
		println("  jmp .L.begin.%d", c)
		println("%s:", node.brkLabel)
		return
	case ND_DO:
		c := count()
		println(".L.begin.%d:", c)
		a.genStmt(node.then)
		println("%s:", node.contLabel)
		a.genExpr(node.cond)
		println("  cmp $0, %%rax")
		println("  jne .L.begin.%d", c)
		println("%s:", node.brkLabel)
		return
	case ND_SWITCH:
		a.genExpr(node.cond)

		for n := node.caseNext; n != nil; n = n.caseNext {
			var reg string
			if node.cond.ty.size == 8 {
				reg = "%rax"
			} else {
				reg = "%eax"
			}
			println("  cmp $%d, %s", n.val, reg)
			println("  je %s", n.label)
		}

		if node.defaultCase != nil {
			println("  jmp %s", node.defaultCase.label)
		}

		println("  jmp %s", node.brkLabel)
		a.genStmt(node.then)
		println("%s:", node.brkLabel)
		return
	case ND_CASE:
		println("%s:", node.label)
		a.genStmt(node.lhs)
		return
	case ND_BLOCK:
		for n := node.body; n != nil; n = n.next {
			a.genStmt(n)
		}
		return
	case ND_GOTO:
		println("  jmp %s", node.uniqueLabel)
		return
	case ND_LABEL:
		println("%s:", node.uniqueLabel)
		a.genStmt(node.lhs)
		return
	case ND_RETURN:
		if node.lhs != nil {
			a.genExpr(node.lhs)
		}
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

		// Save arg registers if function is variadic
		if fn.vaArea != nil {
			gp := 0
			for vara := fn.params; vara != nil; vara = vara.next {
				gp++
			}
			off := fn.vaArea.offset

			// va_elem
			println("  movl $%d, %d(%%rbp)", gp*8, off)
			println("  movl $0, %d(%%rbp)", off+4)
			println("  movq %%rbp, %d(%%rbp)", off+16)
			println("  addq $%d, %d(%%rbp)", off+24, off+16)

			// __reg_save_area__
			println("  movq %%rdi, %d(%%rbp)", off+24)
			println("  movq %%rsi, %d(%%rbp)", off+32)
			println("  movq %%rdx, %d(%%rbp)", off+40)
			println("  movq %%rcx, %d(%%rbp)", off+48)
			println("  movq %%r8, %d(%%rbp)", off+56)
			println("  movq %%r9, %d(%%rbp)", off+64)
			println("  movsd %%xmm0, %d(%%rbp)", off+72)
			println("  movsd %%xmm1, %d(%%rbp)", off+80)
			println("  movsd %%xmm2, %d(%%rbp)", off+88)
			println("  movsd %%xmm3, %d(%%rbp)", off+96)
			println("  movsd %%xmm4, %d(%%rbp)", off+104)
			println("  movsd %%xmm5, %d(%%rbp)", off+112)
			println("  movsd %%xmm6, %d(%%rbp)", off+120)
			println("  movsd %%xmm7, %d(%%rbp)", off+128)
		}

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

func (a X64) emitData(prog *Obj) {
	for vara := prog; vara != nil; vara = vara.next {
		if vara.isFunction || !vara.isDefinition {
			continue
		}

		if vara.isStatic {
			println("  .local %s", vara.name)
		} else {
			println("  .globl %s", vara.name)
		}
		println("  .align %d", vara.align)

		if len(vara.initData) > 0 {
			println("  .data")
			println("%s:", vara.name)

			rel := vara.rel
			pos := 0
			for pos < vara.ty.size {
				if rel != nil && rel.offset == pos {
					println("  .quad %s%+d", rel.label, rel.addend)
					rel = rel.next
					pos += 8
				} else {
					println("  .byte %d", vara.initData[pos])
					pos++
				}
			}
			continue
		}

		println("  .bss")
		println("%s:", vara.name)
		println("  .zero %d", vara.ty.size)
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

	println("  li t0, -%d", stackSize)
	println("  add sp, sp, t0")
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

func (a RiscV) pushf() {
	println("  addi sp, sp, -8")
	println("  fsd fa0, 0(sp)")
	depth++
}

func (a RiscV) popf(arg string) {
	println("  fld %s, 0(sp)", arg)
	println("  addi sp,sp,8")
	depth--
}

// Load a value from where %rax is pointing to.
func (a RiscV) load(ty *Type) {
	switch ty.kind {
	case TY_ARRAY, TY_STRUCT, TY_UNION:
		return
	case TY_FLOAT:
		println("  flw fa0, 0(a0)")
		return
	case TY_DOUBLE:
		println("  fld fa0, 0(a0)")
		return
	}

	suffix := ""
	if ty.isUnsigned {
		suffix = "u"
	}

	switch ty.size {
	case 1:
		println("  lb%s a0, 0(a0)", suffix)
	case 2:
		println("  lh%s a0, 0(a0)", suffix)
	case 4:
		println("  lw%s a0, 0(a0)", suffix)
	default:
		println("  ld a0, 0(a0)")
	}
}

// Store %rax to an address that the stack top is pointing to.
func (a RiscV) store(ty *Type) {
	a.pop("a1")

	switch ty.kind {
	case TY_STRUCT, TY_UNION:
		for i := range ty.size {
			println("  li t0, %d", i)
			println("  add t0, a0, t0")
			println("  lb t1, 0(t0)")

			println("  li t0, %d", i)
			println("  add t0, a1, t0")
			println("  sb t1, 0(t0)")
		}
		return
	case TY_FLOAT:
		println("  fsw fa0, 0(a1)")
		return
	case TY_DOUBLE:
		println("  fsd fa0, 0(a1)")
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
	println("  li t0, %d", offset)
	println("  add t0, fp, t0")

	switch sz {
	case 1:
		println("  sb %s, 0(t0)", argRegR[r])
		return
	case 2:
		println("  sh %s, 0(t0)", argRegR[r])
		return
	case 4:
		println("  sw %s, 0(t0)", argRegR[r])
		return
	case 8:
		println("  sd %s, 0(t0)", argRegR[r])
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
			println("  li t0, %d", node.vara.offset)
			println("  add a0, fp, t0")
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
	case ND_NULL_EXPR:
		return
	case ND_NUM:
		switch node.ty.kind {
		case TY_FLOAT:
			f32 := float32(node.fval)
			u32 := math.Float32bits(f32)
			println("  li a0, %d  # float %f", u32, node.fval)
			println("  fmv.w.x fa0, a0")
			return
		case TY_DOUBLE:
			f64 := node.fval
			u64 := math.Float64bits(f64)
			println("  li a0, %d  # double %f", u64, node.fval)
			println("  fmv.d.x fa0, a0")
			return
		}

		println("  li a0, %d", node.val)
		return
	case ND_NEG:
		a.genExpr(node.lhs)

		switch node.ty.kind {
		case TY_FLOAT:
			println("  fneg.s fa0, fa0")
			return
		case TY_DOUBLE:
			println("  fneg.d fa0, fa0")
			return
		}

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
	case ND_MEMZERO:
		for i := range node.vara.ty.size {
			println("  li t0, %d", node.vara.offset+i)
			println("  add t0, fp, t0")
			println("  sb zero, 0(t0)")
		}
		return
	case ND_COND:
		c := count()
		a.genExpr(node.cond)
		println("  beqz a0, .L.else.%d", c)
		a.genExpr(node.then)
		println("  j .L.end.%d", c)
		println(".L.else.%d:", c)
		a.genExpr(node.els)
		println(".L.end.%d:", c)
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

		if depth%2 == 0 {
			println("  call %s", node.funcname)
		} else {
			println("  addi sp, sp, -8")
			println("  call %s", node.funcname)
			println("  addi sp, sp, 8")
		}

		// It looks like the most significant 48 or 56 bits in RAX may
		// contain garbage if a function return type is short or bool/char,
		// respectively. We clear the upper bits here.
		switch node.ty.kind {
		case TY_BOOL:
			println("  slli a0, a0, 63")
			println("  srli a0, a0, 63")
			return
		case TY_CHAR:
			if node.ty.isUnsigned {
				println("  slli a0, a0, 56")
				println("  srli a0, a0, 56")
			} else {
				println("  slli a0, a0, 56")
				println("  srai a0, a0, 56")
			}
			return
		case TY_SHORT:
			if node.ty.isUnsigned {
				println("  slli a0, a0, 48")
				println("  srli a0, a0, 48")
			} else {
				println("  slli a0, a0, 48")
				println("  srai a0, a0, 48")
			}
			return
		}
		return
	}

	if node.lhs.ty.isFlonum() {
		a.genExpr(node.rhs)
		a.pushf()
		a.genExpr(node.lhs)
		a.popf("fa1")

		var suffix string
		if node.lhs.ty.kind == TY_FLOAT {
			suffix = "s"
		} else {
			suffix = "d"
		}

		switch node.kind {
		case ND_ADD:
			println("  fadd.%s fa0, fa0, fa1", suffix)
			return
		case ND_SUB:
			println("  fsub.%s fa0, fa0, fa1", suffix)
			return
		case ND_MUL:
			println("  fmul.%s fa0, fa0, fa1", suffix)
			return
		case ND_DIV:
			println("  fdiv.%s fa0, fa0, fa1", suffix)
			return
		case ND_EQ:
			println("  feq.%s a0, fa0, fa1", suffix)
			return
		case ND_NE:
			println("  feq.%s a0, fa0, fa1", suffix)
			println("  seqz a0, a0")
			return
		case ND_LT:
			println("  flt.%s a0, fa0, fa1", suffix)
			return
		case ND_LE:
			println("  fle.%s a0, fa0, fa1", suffix)
			return
		}

		failTok(node.tok, "invalid expression")
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
		if node.ty.isUnsigned {
			println("  divu%s a0, a0, a1", suffix)
		} else {
			println("  div%s a0, a0, a1", suffix)
		}
		return
	case ND_MOD:
		if node.ty.isUnsigned {
			println("  remu%s a0, a0, a1", suffix)
		} else {
			println("  rem%s a0, a0, a1", suffix)
		}
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
		if node.lhs.ty.isUnsigned && node.lhs.ty.kind == TY_INT {
			println("slli a0, a0, 32")
			println("srli a0, a0, 32")
		}

		if node.rhs.ty.isUnsigned && node.rhs.ty.kind == TY_INT {
			println("slli a1, a1, 32")
			println("srli a1, a1, 32")
		}

		println("  xor a0, a0, a1")

		if node.kind == ND_EQ {
			println("  seqz a0, a0")
		} else {
			println("  snez a0, a0")
		}

		return
	case ND_LT:
		if node.lhs.ty.isUnsigned {
			println("  sltu a0, a0, a1")
		} else {
			println("  slt a0, a0, a1")
		}
		return
	case ND_LE:
		if node.lhs.ty.isUnsigned {
			println("  sltu a0, a1, a0")
		} else {
			println("  slt a0, a1, a0")
		}
		println("  xori a0, a0, 1")
		return
	case ND_SHL:
		println("  sll%s a0, a0, a1", suffix)
		return
	case ND_SHR:
		if node.ty.isUnsigned {
			println("  srl%s a0, a0, a1", suffix)
		} else {
			println("  sra%s a0, a0, a1", suffix)
		}
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
			println("  beqz a0, %s", node.brkLabel)
		}
		a.genStmt(node.then)
		println("%s:", node.contLabel)
		if node.inc != nil {
			a.genExpr(node.inc)
		}
		println("  j .L.begin.%d", c)
		println("%s:", node.brkLabel)
		return
	case ND_DO:
		c := count()
		println(".L.begin.%d:", c)
		a.genStmt(node.then)
		println("%s:", node.contLabel)
		a.genExpr(node.cond)
		println("  bnez a0, .L.begin.%d", c)
		println("%s:", node.brkLabel)
		return
	case ND_SWITCH:
		a.genExpr(node.cond)

		for n := node.caseNext; n != nil; n = n.caseNext {
			println("  li t0, %d", n.val)
			println("  beq a0, t0, %s", n.label)
		}

		if node.defaultCase != nil {
			println("  j %s", node.defaultCase.label)
		}

		println("  j %s", node.brkLabel)
		a.genStmt(node.then)
		println("%s:", node.brkLabel)
		return
	case ND_CASE:
		println("%s:", node.label)
		a.genStmt(node.lhs)
		return
	case ND_BLOCK:
		for n := node.body; n != nil; n = n.next {
			a.genStmt(n)
		}
		return
	case ND_GOTO:
		println("  j %s", node.uniqueLabel)
		return
	case ND_LABEL:
		println("%s:", node.uniqueLabel)
		a.genStmt(node.lhs)
		return
	case ND_RETURN:
		if node.lhs != nil {
			a.genExpr(node.lhs)
		}
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

		if fn.vaArea != nil {
			offset := fn.vaArea.offset
			for i < 8 {
				a.storeGP(i, offset, 8)
				i++
				offset += 8
			}
		}

		// Emit code
		a.genStmt(fn.body)
		assert(depth == 0)

		// Epilogue
		a.epilogue(fn.name)
	}
}

func (a RiscV) emitData(prog *Obj) {
	for vara := prog; vara != nil; vara = vara.next {
		if vara.isFunction || !vara.isDefinition {
			continue
		}

		if vara.isStatic {
			println("  .local %s", vara.name)
		} else {
			println("  .globl %s", vara.name)
		}

		if vara.align == 0 {
			fail("align can not be 0!")
		}
		println("  .align %d", simpleLog2(vara.align))

		if len(vara.initData) > 0 {
			println("  .data")
			println("%s:", vara.name)

			rel := vara.rel
			pos := 0
			for pos < vara.ty.size {
				if rel != nil && rel.offset == pos {
					println("  .quad %s%+d", rel.label, rel.addend)
					rel = rel.next
					pos += 8
				} else {
					println("  .byte %d", vara.initData[pos])
					pos++
				}
			}
			continue
		}

		println("  .bss")
		println("%s:", vara.name)
		println("  .zero %d", vara.ty.size)
	}
}

func simpleLog2(num int) int {
	n := num
	e := 0
	for n > 1 {
		if n%2 == 1 {
			fail(fmt.Sprintf("Wrong value %d", num))
		}
		n /= 2
		e++
	}
	return e
}

func codegen(target Arch, prog *Obj, out *os.File) {
	outputFile = out
	assignLVarOffsets(prog)

	target.emitData(prog)
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
			offset = alignTo(offset, vara.align)
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
