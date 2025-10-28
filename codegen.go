package main

import (
	"fmt"
	"io"
	"math"
)

const (
	GP_MAX_X = 6
	FP_MAX_X = 8
)

var ArchName string
var outputFile io.Writer
var depth int
var currentGenFn *Obj

var argReg8x = []string{"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"}
var argReg16x = []string{"%di", "%si", "%dx", "%cx", "%r8w", "%r9w"}
var argReg32x = []string{"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"}
var argReg64x = []string{"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"}

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
	F80
)

// The table for x64 type casts
var i32i8x = "movsbl %al, %eax"
var i32u8x = "movzbl %al, %eax"
var i32i16x = "movswl %ax, %eax"
var i32u16x = "movzwl %ax, %eax"
var i32f32x = "cvtsi2ssl %eax, %xmm0"
var i32i64x = "movsxd %eax, %rax"
var i32f64x = "cvtsi2sdl %eax, %xmm0"
var i32f80x = "mov %eax, -4(%rsp); fildl -4(%rsp)"

var u32f32x = "mov %eax, %eax; cvtsi2ssq %rax, %xmm0"
var u32i64x = "mov %eax, %eax"
var u32f64x = "mov %eax, %eax; cvtsi2sdq %rax, %xmm0"
var u32f80x = "mov %eax, %eax; mov %rax, -8(%rsp); fildll -8(%rsp)"

var i64f32x = "cvtsi2ssq %rax, %xmm0"
var i64f64x = "cvtsi2sdq %rax, %xmm0"
var i64f80x = "movq %rax, -8(%rsp); fildll -8(%rsp)"

var u64f32x = "cvtsi2ssq %rax, %xmm0"
var u64f64x = "test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %rax,%xmm0; jmp 2f; " +
	"1: mov %rax,%rdi; and $1,%eax; pxor %xmm0,%xmm0; shr %rdi; " +
	"or %rax,%rdi; cvtsi2sd %rdi,%xmm0; addsd %xmm0,%xmm0; 2:"
var u64f80x = "mov %rax, -8(%rsp); fildq -8(%rsp); test %rax, %rax; jns 1f;" +
	"mov $1602224128, %eax; mov %eax, -4(%rsp); fadds -4(%rsp); 1:"

var f32i8x = "cvttss2sil %xmm0, %eax; movsbl %al, %eax"
var f32u8x = "cvttss2sil %xmm0, %eax; movzbl %al, %eax"
var f32i16x = "cvttss2sil %xmm0, %eax; movswl %ax, %eax"
var f32u16x = "cvttss2sil %xmm0, %eax; movzwl %ax, %eax"
var f32i32x = "cvttss2sil %xmm0, %eax"
var f32u32x = "cvttss2siq %xmm0, %rax"
var f32i64x = "cvttss2siq %xmm0, %rax"
var f32u64x = "cvttss2siq %xmm0, %rax"
var f32f64x = "cvtss2sd %xmm0, %xmm0"
var f32f80x = "movss %xmm0, -4(%rsp); flds -4(%rsp)"

var f64i8x = "cvttsd2sil %xmm0, %eax; movsbl %al, %eax"
var f64u8x = "cvttsd2sil %xmm0, %eax; movzbl %al, %eax"
var f64i16x = "cvttsd2sil %xmm0, %eax; movswl %ax, %eax"
var f64u16x = "cvttsd2sil %xmm0, %eax; movzwl %ax, %eax"
var f64i32x = "cvttsd2sil %xmm0, %eax"
var f64u32x = "cvttsd2siq %xmm0, %rax"
var f64i64x = "cvttsd2siq %xmm0, %rax"
var f64u64x = "cvttsd2siq %xmm0, %rax"
var f64f32x = "cvtsd2ss %xmm0, %xmm0"
var f64f80x = "movsd %xmm0, -8(%rsp); fldl -8(%rsp)"

var FROM_F80_1 = "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; " +
	"mov %ax, -12(%rsp); fldcw -12(%rsp); "
var FROM_F80_2 = " -24(%rsp); fldcw -10(%rsp); "

var f80i8x = FROM_F80_1 + "fistps" + FROM_F80_2 + "movsbl -24(%rsp), %eax"
var f80u8x = FROM_F80_1 + "fistps" + FROM_F80_2 + "movzbl -24(%rsp), %eax"
var f80i16x = FROM_F80_1 + "fistps" + FROM_F80_2 + "movzbl -24(%rsp), %eax"
var f80u16x = FROM_F80_1 + "fistpl" + FROM_F80_2 + "movswl -24(%rsp), %eax"
var f80i32x = FROM_F80_1 + "fistpl" + FROM_F80_2 + "mov -24(%rsp), %eax"
var f80u32x = FROM_F80_1 + "fistpl" + FROM_F80_2 + "mov -24(%rsp), %eax"
var f80i64x = FROM_F80_1 + "fistpq" + FROM_F80_2 + "mov -24(%rsp), %rax"
var f80u64x = FROM_F80_1 + "fistpq" + FROM_F80_2 + "mov -24(%rsp), %rax"
var f80f32x = "fstps -8(%rsp); movss -8(%rsp), %xmm0"
var f80f64x = "fstpl -8(%rsp); movsd -8(%rsp), %xmm0"

var x64CastTable = [][]string{
	// i8   i16   i32   i64   u8   u16   u32   u64   f32   f64   f80
	{"", "", "", i32i64x, i32u8x, i32u16x, "", i32i64x, i32f32x, i32f64x, i32f80x},               // i8
	{i32i8x, "", "", i32i64x, i32u8x, i32u16x, "", i32i64x, i32f32x, i32f64x, i32f80x},           // i16
	{i32i8x, i32i16x, "", i32i64x, i32u8x, i32u16x, "", i32i64x, i32f32x, i32f64x, i32f80x},      // i32
	{i32i8x, i32i16x, "", "", i32u8x, i32u16x, "", "", i64f32x, i64f64x, i64f80x},                // i64
	{i32i8x, "", "", i32i64x, "", "", "", i32i64x, i32f32x, i32f64x, i32f80x},                    // u8
	{i32i8x, i32i16x, "", i32i64x, i32u8x, "", "", i32i64x, i32f32x, i32f64x, i32f80x},           // u16
	{i32i8x, i32i16x, "", u32i64x, i32u8x, i32u16x, "", u32i64x, u32f32x, u32f64x, u32f80x},      // u32
	{i32i8x, i32i16x, "", "", i32u8x, i32u16x, "", "", u64f32x, u64f64x, u64f80x},                // u64
	{f32i8x, f32i16x, f32i32x, f32i64x, f32u8x, f32u16x, f32u32x, f32u64x, "", f32f64x, f32f80x}, // f32
	{f64i8x, f64i16x, f64i32x, f64i64x, f64u8x, f64u16x, f64u32x, f64u64x, f64f32x, "", f64f80x}, // f64
	{f80i8x, f80i16x, f80i32x, f80i64x, f80u8x, f80u16x, f80u32x, f80u64x, f80f32x, f80f64x, ""}, // f80
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
	case TY_LDOUBLE:
		return F80
	}
	return U64
}

func chooseArch(arch string) Arch {
	var target Arch
	switch arch {
	case "x64":
		target = &X64{}
	default:
		fail("unsupported architecture: %s", arch)
	}
	ArchName = arch
	return target
}

type Arch interface {
	emitText(prog *Obj)
	emitData(prog *Obj)
	assemble(input, output string)
	runLinker(inputs []string, output string)
	addDefaultIncludePaths(argv0 string)
	initMacro()
	assignLVarOffsets(prog *Obj)
}

type X64 struct{}

func (a X64) prologue(fname string, stackSize int, isStatic bool) {
	if isStatic {
		println("  .local %s", fname)
	} else {
		println("  .globl %s", fname)
	}

	println("  .text")
	println("  .type %s, @function", fname)
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

func (a X64) popf(reg int) {
	println("  movsd (%%rsp), %%xmm%d", reg)
	println("  add $8, %%rsp")
	depth--
}

// Load a value from where %rax is pointing to.
func (a X64) load(ty *Type) {
	switch ty.kind {
	case TY_ARRAY, TY_STRUCT, TY_UNION, TY_FUNC, TY_VLA:
		return
	case TY_FLOAT:
		println("  movss (%%rax), %%xmm0")
		return
	case TY_DOUBLE:
		println("  movsd (%%rax), %%xmm0")
		return
	case TY_LDOUBLE:
		println("  fldt (%%rax)")
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
	case TY_LDOUBLE:
		println("  fstpt (%%rdi)")
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
	default:
		for i := range sz {
			println("  mov %s, %d(%%rbp)", argReg8x[r], offset+i)
			println("  shr $8, %s", argReg64x[r])
		}
		return
	}
}

func (a X64) storeFP(r, offset, sz int) {
	switch sz {
	case 4:
		println("  movss %%xmm%d, %d(%%rbp)", r, offset)
		return
	case 8:
		println("  movsd %%xmm%d, %d(%%rbp)", r, offset)
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
	switch ty.kind {
	case TY_FLOAT:
		println("  xorps %%xmm1, %%xmm1")
		println("  ucomiss %%xmm1, %%xmm0")
		return
	case TY_DOUBLE:
		println("  xorpd %%xmm1, %%xmm1")
		println("  ucomisd %%xmm1, %%xmm0")
		return
	case TY_LDOUBLE:
		println("  fldz")
		println("  fucomip")
		println("  fstp %%st(0)")
		return
	}

	if ty.isInteger() && ty.size <= 4 {
		println("  cmp $0, %%eax")
	} else {
		println("  cmp $0, %%rax")
	}
}

// Structs or unions equal or smaller than 16 bytes are passed
// using up to two registers.
//
// If the first 8 bytes contains only floating-point type members,
// they are passed in an XMM register. Otherwise, they are passed
// in a general-purpose register.
//
// If a struct/union is larger than 8 bytes, the same rule is
// applied to the the next 8 byte chunk.
//
// This function returns true if `ty` has only floating-point
// members in its byte range [lo, hi).
func hasFlonum(ty *Type, lo, hi, offset int) bool {
	if ty.kind == TY_STRUCT || ty.kind == TY_UNION {
		for mem := ty.members; mem != nil; mem = mem.next {
			if !hasFlonum(mem.ty, lo, hi, offset+mem.offset) {
				return false
			}
		}
		return true
	}

	if ty.kind == TY_ARRAY {
		for i := 0; i < ty.arrayLen; i++ {
			if !hasFlonum(ty.base, lo, hi, offset+ty.base.size*i) {
				return false
			}
		}
		return true
	}

	return offset < lo || hi < offset || ty.kind == TY_FLOAT || ty.kind == TY_DOUBLE
}

func hasFlonum1(ty *Type) bool {
	return hasFlonum(ty, 0, 8, 0)
}

func hasFlonum2(ty *Type) bool {
	return hasFlonum(ty, 8, 16, 0)
}

func (a X64) pushStruct(ty *Type) {
	sz := alignTo(ty.size, 8)
	println("  sub $%d, %%rsp", sz)
	depth += sz / 8

	for i := 0; i < ty.size; i++ {
		println("  mov %d(%%rax), %%r10b", i)
		println("  mov %%r10b, %d(%%rsp)", i)
	}
}

func (a X64) pushArgs2(args *Node, firstPass bool) {
	if args == nil {
		return
	}

	a.pushArgs2(args.next, firstPass)

	if (firstPass && !args.passByStack) || (!firstPass && args.passByStack) {
		return
	}

	a.genExpr(args)

	switch args.ty.kind {
	case TY_STRUCT, TY_UNION:
		a.pushStruct(args.ty)
	case TY_FLOAT, TY_DOUBLE:
		a.pushf()
	case TY_LDOUBLE:
		println("  sub $16, %%rsp")
		println("  fstpt (%%rsp)")
		depth += 2
	default:
		a.push()
	}
}

// Load function call arguments. Arguments are already evaluated and
// stored to the stack as local variables. What we need to do in this
// function is to load them to registers or push them to the stack as
// specified by the x86-64 psABI.
func (a X64) pushArgs(node *Node) int {
	stack, gp, fp := 0, 0, 0

	// If the return type is a large struct/union, the caller passes
	// a pointer to a buffer as if it were the first argument.
	if node.retBuffer != nil && node.ty.size > 16 {
		gp++
	}

	// Load as many arguments to the registers as possible.
	for arg := node.args; arg != nil; arg = arg.next {
		ty := arg.ty

		switch ty.kind {
		case TY_STRUCT, TY_UNION:
			if ty.size > 16 {
				arg.passByStack = true
				stack += alignTo(ty.size, 8) / 8
			} else {
				fp1b := hasFlonum1(ty)
				fp2b := hasFlonum2(ty)
				var fp1, fp2, nfp1, nfp2 int
				if fp1b {
					fp1 = 1
				} else {
					nfp1 = 1
				}
				if fp2b {
					fp2 = 1
				} else {
					nfp2 = 1
				}

				if fp+fp1+fp2 < FP_MAX_X && gp+nfp1+nfp2 < GP_MAX_X {
					fp = fp + fp1 + fp2
					gp = gp + nfp1 + nfp2
				} else {
					arg.passByStack = true
					stack += alignTo(ty.size, 8) / 8
				}
			}
		case TY_FLOAT, TY_DOUBLE:
			if fp >= FP_MAX_X {
				arg.passByStack = true
				stack++
			}
			fp++
		case TY_LDOUBLE:
			arg.passByStack = true
			stack += 2
		default:
			if gp >= GP_MAX_X {
				arg.passByStack = true
				stack++
			}
			gp++
		}
	}

	if (depth+stack)%2 == 1 {
		println("  sub $8, %%rsp")
		depth++
		stack++
	}

	a.pushArgs2(node.args, true)
	a.pushArgs2(node.args, false)

	// If the return type is a large struct/union, the caller passes
	// a pointer to a buffer as if it were the first argument.
	if node.retBuffer != nil && node.ty.size > 16 {
		println("  lea %d(%%rbp), %%rax", node.retBuffer.offset)
		a.push()
	}

	return stack
}

func (a X64) copyRetBuffer(vara *Obj) {
	ty := vara.ty
	gp, fp := 0, 0

	if hasFlonum1(ty) {
		assert(ty.size == 4 || 8 <= ty.size)
		if ty.size == 4 {
			println("  movss %%xmm0, %d(%%rbp)", vara.offset)
		} else {
			println("  movsd %%xmm0, %d(%%rbp)", vara.offset)
		}
		fp++
	} else {
		for i := range min(8, ty.size) {
			println("  mov %%al, %d(%%rbp)", vara.offset+i)
			println("  shr $8, %%rax")
		}
		gp++
	}

	if ty.size > 8 {
		if hasFlonum2(ty) {
			assert(ty.size == 12 || ty.size == 16)
			if ty.size == 12 {
				println("  movss %%xmm%d, %d(%%rbp)", fp, vara.offset+8)
			} else {
				println("  movsd %%xmm%d, %d(%%rbp)", fp, vara.offset+8)
			}
		} else {
			var reg1, reg2 string
			if gp == 0 {
				reg1 = "%al"
				reg2 = "%rax"
			} else {
				reg1 = "%dl"
				reg2 = "%rdx"
			}
			for i := 8; i < min(16, ty.size); i++ {
				println("  mov %s, %d(%%rbp)", reg1, vara.offset+i)
				println("  shr $8, %s", reg2)
			}
		}
	}
}

func (a X64) copyStructReg() {
	ty := currentGenFn.ty.returnTy
	gp, fp := 0, 0

	println("  mov %%rax, %%rdi")

	if hasFlonum(ty, 0, 8, 0) {
		assert(ty.size == 4 || 8 <= ty.size)
		if ty.size == 4 {
			println("  movss (%%rdi), %%xmm0")
		} else {
			println("  movsd (%%rdi), %%xmm0")
		}
		fp++
	} else {
		println("  mov $0, %%rax")
		for i := min(8, ty.size) - 1; i >= 0; i-- {
			println("  shl $8, %%rax")
			println("  mov %d(%%rdi), %%al", i)
		}
		gp++
	}

	if ty.size > 8 {
		if hasFlonum(ty, 8, 16, 0) {
			assert(ty.size == 12 || ty.size == 16)
			if ty.size == 4 {
				println("  movss 8(%%rdi), %%xmm%d", fp)
			} else {
				println("  movsd 8(%%rdi), %%xmm%d", fp)
			}
		} else {
			var reg1, reg2 string
			if gp == 0 {
				reg1 = "%al"
				reg2 = "%rax"
			} else {
				reg1 = "%dl"
				reg2 = "%rdx"
			}
			println("  mov $0, %s", reg2)
			for i := min(16, ty.size) - 1; i >= 8; i-- {
				println("  shl $8, %s", reg2)
				println("  mov %d(%%rdi), %s", i, reg1)
			}
		}
	}
}

func (a X64) copyStructMem() {
	ty := currentGenFn.ty.returnTy
	vara := currentGenFn.params

	println("  mov %d(%%rbp), %%rdi", vara.offset)

	for i := 0; i < ty.size; i++ {
		println("  mov %d(%%rax), %%dl", i)
		println("  mov %%dl, %d(%%rdi)", i)
	}
}

func (a X64) regDx(sz int) string {
	switch sz {
	case 1:
		return "%dl"
	case 2:
		return "%dx"
	case 4:
		return "%edx"
	case 8:
		return "%rdx"
	}
	unreachable()
	return ""
}

func (a X64) regAx(sz int) string {
	switch sz {
	case 1:
		return "%al"
	case 2:
		return "%ax"
	case 4:
		return "%eax"
	case 8:
		return "%rax"
	}
	unreachable()
	return ""
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
func (a X64) genAddr(node *Node) {
	switch node.kind {
	case ND_VAR:
		// Variable-length array, which is always local.
		if node.vara.ty.kind == TY_VLA {
			println("  mov %d(%%rbp), %%rax", node.vara.offset)
			return
		}

		// Local variable
		if node.vara.isLocal {
			println("  lea %d(%%rbp), %%rax", node.vara.offset)
			return
		}

		if optFpic {
			// Thread-local variable
			if node.vara.isTls {
				println("  data16 lea %s@tlsgd(%%rip), %%rdi", node.vara.name)
				println("  .value 0x6666")
				println("  rex64")
				println("  call __tls_get_addr@PLT")
				return
			}

			// Function or global variable
			println("  mov %s@GOTPCREL(%%rip), %%rax", node.vara.name)
			return
		}

		// Thread-local variable
		if node.vara.isTls {
			println("  mov %%fs:0, %%rax")
			println("  add $%s@tpoff, %%rax", node.vara.name)
			return
		}

		// Function
		if node.ty.kind == TY_FUNC {
			if node.vara.isDefinition {
				println("  lea %s(%%rip), %%rax", node.vara.name)
			} else {
				println("  mov %s@GOTPCREL(%%rip), %%rax", node.vara.name)
			}
			return
		}

		// Global variable
		println("  lea %s(%%rip), %%rax", node.vara.name)
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
	case ND_FUNCALL:
		if node.retBuffer != nil {
			a.genExpr(node)
			return
		}
	case ND_ASSIGN, ND_COND:
		if node.ty.kind == TY_STRUCT || node.ty.kind == TY_UNION {
			a.genExpr(node)
			return
		}
	case ND_VLA_PTR:
		println("  lea %d(%%rbp), %%rax", node.vara.offset)
		return
	}

	failTok(node.tok, "not an lvalue")
}

func (a X64) builtinAlloca() {
	// Align size to 16 bytes.
	println("  add $15, %%rdi")
	println("  and $0xfffffff0, %%edi")

	// Shift the temporary area by %rdi.
	println("  mov %d(%%rbp), %%rcx", currentGenFn.allocaBottom.offset)
	println("  sub %%rsp, %%rcx")
	println("  mov %%rsp, %%rax")
	println("  sub %%rdi, %%rsp")
	println("  mov %%rsp, %%rdx")
	println("1:")
	println("  cmp $0, %%rcx")
	println("  je 2f")
	println("  mov (%%rax), %%r8b")
	println("  mov %%r8b, (%%rdx)")
	println("  inc %%rdx")
	println("  inc %%rax")
	println("  dec %%rcx")
	println("  jmp 1b")
	println("2:")

	// Move alloca_bottom pointer.
	println("  mov %d(%%rbp), %%rax", currentGenFn.allocaBottom.offset)
	println("  sub %%rdi, %%rax")
	println("  mov %%rax, %d(%%rbp)", currentGenFn.allocaBottom.offset)
}

// Generate code for a given node.
func (a X64) genExpr(node *Node) {
	println("  .loc %d %d", node.tok.file.fileno, node.tok.lineno)

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
		case TY_LDOUBLE:
			f64 := node.fval
			u64 := math.Float64bits(f64)
			println("  mov $%d, %%rax  # long double %f", u64, node.fval)
			println("  mov %%rax, -16(%%rsp)")
			println("  mov $%d, %%rax", 0)
			println("  mov %%rax, -8(%%rsp)")
			println("  fldt -16(%%rsp)")
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
		case TY_LDOUBLE:
			println("  fchs")
			return
		}

		println("  neg %%rax")
		return
	case ND_VAR:
		a.genAddr(node)
		a.load(node.ty)
		return
	case ND_MEMBER:
		a.genAddr(node)
		a.load(node.ty)

		mem := node.member
		if mem.isBitfield {
			println("  shl $%d, %%rax", 64-mem.bitWidth-mem.bitOffset)
			if mem.ty.isUnsigned {
				println("  shr $%d, %%rax", 64-mem.bitWidth)
			} else {
				println("  sar $%d, %%rax", 64-mem.bitWidth)
			}
		}
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

		if node.lhs.kind == ND_MEMBER && node.lhs.member.isBitfield {
			println("  mov %%rax, %%r8")

			// If the lhs is a bitfield, we need to read the current value
			// from memory and merge it with a new value.
			mem := node.lhs.member

			println("  mov %%rax, %%rdi")
			println("  and $%d, %%rdi", (1<<mem.bitWidth)-1)
			println("  shl $%d, %%rdi", mem.bitOffset)

			println("  mov (%%rsp), %%rax")
			a.load(mem.ty)

			mask := ((1 << mem.bitWidth) - 1) << mem.bitOffset
			println("  mov $%d, %%r9", ^mask)
			println("  and %%r9, %%rax")
			println("  or %%rdi, %%rax")
			a.store(node.ty)
			println("  mov %%r8, %%rax")
			return
		}

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
		a.cmpZero(node.cond.ty)
		println("  je .L.else.%d", c)
		a.genExpr(node.then)
		println("  jmp .L.end.%d", c)
		println(".L.else.%d:", c)
		a.genExpr(node.els)
		println(".L.end.%d:", c)
		return
	case ND_NOT:
		a.genExpr(node.lhs)
		a.cmpZero(node.lhs.ty)
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
		a.cmpZero(node.lhs.ty)
		println("  je .L.false.%d", c)
		a.genExpr(node.rhs)
		a.cmpZero(node.rhs.ty)
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
		a.cmpZero(node.lhs.ty)
		println("  jne .L.true.%d", c)
		a.genExpr(node.rhs)
		a.cmpZero(node.rhs.ty)
		println("  jne .L.true.%d", c)
		println("  mov $0, %%rax")
		println("  jmp .L.end.%d", c)
		println(".L.true.%d:", c)
		println("  mov $1, %%rax")
		println(".L.end.%d:", c)
		return
	case ND_FUNCALL:
		if node.lhs.kind == ND_VAR && node.lhs.vara.name == "alloca" {
			a.genExpr(node.args)
			println("  mov %%rax, %%rdi")
			a.builtinAlloca()
			return
		}

		stackArgs := a.pushArgs(node)
		a.genExpr(node.lhs)
		gp, fp := 0, 0

		// If the return type is a large struct/union, the caller passes
		// a pointer to a buffer as if it were the first argument.
		if node.retBuffer != nil && node.ty.size > 16 {
			a.pop(argReg64x[gp])
			gp++
		}

		for arg := node.args; arg != nil; arg = arg.next {
			ty := arg.ty

			switch ty.kind {
			case TY_STRUCT, TY_UNION:
				if ty.size > 16 {
					continue
				}

				fp1b := hasFlonum1(ty)
				fp2b := hasFlonum2(ty)
				var fp1, fp2, nfp1, nfp2 int
				if fp1b {
					fp1 = 1
				} else {
					nfp1 = 1
				}
				if fp2b {
					fp2 = 1
				} else {
					nfp2 = 1
				}

				if fp+fp1+fp2 < FP_MAX_X && gp+nfp1+nfp2 < GP_MAX_X {
					if fp1b {
						a.popf(fp)
						fp++
					} else {
						a.pop(argReg64x[gp])
						gp++
					}

					if ty.size > 8 {
						if fp2b {
							a.popf(fp)
							fp++
						} else {
							a.pop(argReg64x[gp])
							gp++
						}
					}
				}
			case TY_FLOAT, TY_DOUBLE:
				if fp < FP_MAX_X {
					a.popf(fp)
					fp++
				}
			case TY_LDOUBLE:
			default:
				if gp < GP_MAX_X {
					a.pop(argReg64x[gp])
					gp++
				}
			}
		}

		println("  mov %%rax, %%r10")
		println("  mov $%d, %%rax", fp)
		println("  call *%%r10")
		println("  add $%d, %%rsp", stackArgs*8)

		depth -= stackArgs

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

		// If the return type is a small struct, a value is returned
		// using up to two registers.
		if node.retBuffer != nil && node.ty.size <= 16 {
			a.copyRetBuffer(node.retBuffer)
			println("  lea %d(%%rbp), %%rax", node.retBuffer.offset)
		}

		return
	case ND_LABEL_VAL:
		println("  lea %s(%%rip), %%rax", node.uniqueLabel)
		return
	case ND_CAS:
		a.genExpr(node.casAddr)
		a.push()
		a.genExpr(node.casNew)
		a.push()
		a.genExpr(node.casOld)
		println("  mov %%rax, %%r8")
		a.load(node.casOld.ty.base)
		a.pop("%rdx") // new
		a.pop("%rdi") // addr

		sz := node.casAddr.ty.base.size
		println("  lock cmpxchg %s, (%%rdi)", a.regDx(sz))
		println("  sete %%cl")
		println("  je 1f")
		println("  mov %s, (%%r8)", a.regAx(sz))
		println("1:")
		println("  movzbl %%cl, %%eax")
		return
	case ND_EXCH:
		a.genExpr(node.lhs)
		a.push()
		a.genExpr(node.rhs)
		a.pop("%rdi")

		sz := node.lhs.ty.base.size
		println("  xchg %s, (%%rdi)", a.regAx(sz))
		return
	}

	switch node.lhs.ty.kind {
	case TY_FLOAT, TY_DOUBLE:
		a.genExpr(node.rhs)
		a.pushf()
		a.genExpr(node.lhs)
		a.popf(1)

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
	case TY_LDOUBLE:
		a.genExpr(node.lhs)
		a.genExpr(node.rhs)

		switch node.kind {
		case ND_ADD:
			println("  faddp")
			return
		case ND_SUB:
			println("  fsubrp")
			return
		case ND_MUL:
			println("  fmulp")
			return
		case ND_DIV:
			println("  fdivrp")
			return
		case ND_EQ, ND_NE, ND_LT, ND_LE:
			println("  fcomip")
			println("  fstp %%st(0)")

			switch node.kind {
			case ND_EQ:
				println("  sete %%al")
			case ND_NE:
				println("  setne %%al")
			case ND_LT:
				println("  seta %%al")
			default:
				println("  setae %%al")
			}

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
	println("  .loc %d %d", node.tok.file.fileno, node.tok.lineno)

	switch node.kind {
	case ND_IF:
		c := count()
		a.genExpr(node.cond)
		a.cmpZero(node.cond.ty)
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
			a.cmpZero(node.cond.ty)
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
		a.cmpZero(node.cond.ty)
		println("  jne .L.begin.%d", c)
		println("%s:", node.brkLabel)
		return
	case ND_SWITCH:
		a.genExpr(node.cond)

		for n := node.caseNext; n != nil; n = n.caseNext {
			var ax, di string
			if node.cond.ty.size == 8 {
				ax = "%rax"
				di = "%rdi"
			} else {
				ax = "%eax"
				di = "%edi"
			}

			if n.begin == n.end {
				println("  cmp $%d, %s", n.begin, ax)
				println("  je %s", n.label)
				continue
			}

			// [GNU] Case ranges
			println("  mov %s, %s", ax, di)
			println("  sub $%d, %s", n.begin, di)
			println("  cmp $%d, %s", n.end-n.begin, di)
			println("  jbe %s", n.label)
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
	case ND_GOTO_EXPR:
		a.genExpr(node.lhs)
		println("  jmp *%%rax")
		return
	case ND_LABEL:
		println("%s:", node.uniqueLabel)
		a.genStmt(node.lhs)
		return
	case ND_RETURN:
		if node.lhs != nil {
			a.genExpr(node.lhs)
			ty := node.lhs.ty

			switch ty.kind {
			case TY_STRUCT, TY_UNION:
				if ty.size <= 16 {
					a.copyStructReg()
				} else {
					a.copyStructMem()
				}
			}
		}

		println("  jmp .L.return.%s", currentGenFn.name)
		return
	case ND_EXPR_STMT:
		a.genExpr(node.lhs)
		return
	case ND_ASM:
		println("  %s", node.asmStr)
		return
	}

	failTok(node.tok, "invalid statement")
}

func (a X64) emitText(prog *Obj) {
	for fn := prog; fn != nil; fn = fn.next {
		if !fn.isFunction || !fn.isDefinition {
			continue
		}

		// No code is emitted for "static inline" functions
		// if no one is referencing them.
		if !fn.isLive {
			continue
		}

		// Prologue
		a.prologue(fn.name, fn.stackSize, fn.isStatic)
		println("  mov %%rsp, %d(%%rbp)", fn.allocaBottom.offset)
		currentGenFn = fn

		// Save arg registers if function is variadic
		if fn.vaArea != nil {
			gp, fp := 0, 0
			for vara := fn.params; vara != nil; vara = vara.next {
				if vara.ty.isFlonum() {
					fp++
				} else {
					gp++
				}
			}
			off := fn.vaArea.offset

			// va_elem
			println("  movl $%d, %d(%%rbp)", gp*8, off)      // gp_offset
			println("  movl $%d, %d(%%rbp)", fp*8+48, off+4) // fp_offset
			println("  movq %%rbp, %d(%%rbp)", off+8)        // overflow_arg_area
			println("  addq $16, %d(%%rbp)", off+8)
			println("  movq %%rbp, %d(%%rbp)", off+16) // reg_save_area
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
		gp, fp := 0, 0
		for vara := fn.params; vara != nil; vara = vara.next {
			if vara.offset > 0 {
				continue
			}

			ty := vara.ty

			switch ty.kind {
			case TY_STRUCT, TY_UNION:
				assert(ty.size <= 16)
				if hasFlonum(ty, 0, 8, 0) {
					a.storeFP(fp, vara.offset, min(8, ty.size))
					fp++
				} else {
					a.storeGP(gp, vara.offset, min(8, ty.size))
					gp++
				}

				if ty.size > 8 {
					if hasFlonum(ty, 8, 16, 0) {
						a.storeFP(fp, vara.offset+8, ty.size-8)
						fp++
					} else {
						a.storeGP(gp, vara.offset+8, ty.size-8)
						gp++
					}
				}
			case TY_FLOAT, TY_DOUBLE:
				a.storeFP(fp, vara.offset, ty.size)
				fp++
			default:
				a.storeGP(gp, vara.offset, ty.size)
				gp++
			}
		}

		// Emit code
		a.genStmt(fn.body)
		assert(depth == 0)

		// [https://www.sigbus.info/n1570#5.1.2.2.3p1] The C spec defines
		// a special rule for the main function. Reaching the end of the
		// main function is equivalent to returning 0, even though the
		// behavior is undefined for the other functions.
		if fn.name == "main" {
			println("  mov $0, %%rax")
		}

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

		var align int
		if vara.ty.kind == TY_ARRAY && vara.ty.size >= 16 {
			align = max(16, vara.align)
		} else {
			align = vara.align
		}

		// Common symbol
		if optFcommon && vara.isTentative {
			println("  .comm %s, %d, %d", vara.name, vara.ty.size, align)
			continue
		}

		// .data or .tdata
		if len(vara.initData) > 0 {
			if vara.isTls {
				println("  .section .tdata,\"awT\",@progbits")
			} else {
				println("  .data")
			}

			println("  .type %s, @object", vara.name)
			println("  .size %s, %d", vara.name, vara.ty.size)
			println("  .align %d", align)
			println("%s:", vara.name)

			rel := vara.rel
			pos := 0
			for pos < vara.ty.size {
				if rel != nil && rel.offset == pos {
					println("  .quad %s%+d", *rel.label, rel.addend)
					rel = rel.next
					pos += 8
				} else {
					println("  .byte %d", int8(vara.initData[pos]))
					pos++
				}
			}
			continue
		}

		// .bss or .tbss
		if vara.isTls {
			println("  .section .tbss,\"awT\",@nobits")
		} else {
			println("  .bss")
		}

		println("  .align %d", align)
		println("%s:", vara.name)
		println("  .zero %d", vara.ty.size)
	}
}

func codegen(target Arch, prog *Obj, out io.Writer) {
	outputFile = out

	files := inputfiles
	for i := 0; files[i] != nil; i++ {
		println("  .file %d \"%s\"", files[i].fileno, files[i].name)
	}

	target.assignLVarOffsets(prog)

	target.emitData(prog)
	target.emitText(prog)
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
func alignTo(n, align int) int {
	return (n + align - 1) / align * align
}

// Assign offsets to local variables.
func (a X64) assignLVarOffsets(prog *Obj) {
	for fn := prog; fn != nil; fn = fn.next {
		if !fn.isFunction {
			continue
		}

		// If a function has many parameters, some parameters are
		// inevitably passed by stack rather than by register.
		// The first passed-by-stack parameter resides at RBP+16.
		top := 16
		bottom := 0

		gp, fp := 0, 0

		// Assign offsets to pass-by-stack parameters.
		for vara := fn.params; vara != nil; vara = vara.next {
			ty := vara.ty

			switch ty.kind {
			case TY_STRUCT, TY_UNION:
				if ty.size <= 16 {
					fp1b := hasFlonum(ty, 0, 8, 0)
					fp2b := hasFlonum(ty, 8, 16, 8)

					var fp1, fp2, nfp1, nfp2 int
					if fp1b {
						fp1 = 1
					} else {
						nfp1 = 1
					}
					if fp2b {
						fp2 = 1
					} else {
						nfp2 = 1
					}
					if fp+fp1+fp2 < FP_MAX_X && gp+nfp1+nfp2 < GP_MAX_X {
						fp = fp + fp1 + fp2
						gp = gp + nfp1 + nfp2
						continue
					}
				}
			case TY_FLOAT, TY_DOUBLE:
				if fp < FP_MAX_X {
					fp++
					continue
				}
			case TY_LDOUBLE:
			default:
				if gp < GP_MAX_X {
					gp++
					continue
				}
			}

			top = alignTo(top, 8)
			vara.offset = top
			top += vara.ty.size
		}

		// Assign offsets to pass-by-register parameters and local variables.
		for vara := fn.locals; vara != nil; vara = vara.next {
			if vara.offset != 0 {
				continue
			}

			// AMD64 System V ABI has a special alignment rule for an array of
			// length at least 16 bytes. We need to align such array to at least
			// 16-byte boundaries. See p.14 of
			// https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf.
			var align int
			if vara.ty.kind == TY_ARRAY && vara.ty.size >= 16 {
				align = max(16, vara.align)
			} else {
				align = vara.align
			}

			bottom += vara.ty.size
			bottom = alignTo(bottom, align)
			vara.offset = -bottom
		}

		fn.stackSize = alignTo(bottom, 16)
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
