package main

var tyVoid = &Type{kind: TY_VOID, size: 1, align: 1}
var tyBool = &Type{kind: TY_BOOL, size: 1, align: 1}

var tyChar = &Type{kind: TY_CHAR, size: 1, align: 1}
var tyShort = &Type{kind: TY_SHORT, size: 2, align: 2}
var tyInt = &Type{kind: TY_INT, size: 4, align: 4}
var tyLong = &Type{kind: TY_LONG, size: 8, align: 8}

type TypeKind int

const (
	TY_VOID TypeKind = iota
	TY_BOOL
	TY_CHAR
	TY_SHORT
	TY_INT
	TY_LONG
	TY_ENUM
	TY_PTR
	TY_FUNC
	TY_ARRAY
	TY_STRUCT
	TY_UNION
)

type Type struct {
	kind  TypeKind
	size  int // sizeof() value
	align int // alignment

	// Pointer-to or array-of type.
	base *Type

	// Declaration
	name *Token

	// Array
	arrayLen int

	// Struct
	members *Member

	// Function type
	returnTy *Type
	params   *Type
	next     *Type
}

func newType(kind TypeKind, size int, align int) *Type {
	return &Type{
		kind:  kind,
		size:  size,
		align: align,
	}
}

func (ty Type) isInteger() bool {
	k := ty.kind
	return k == TY_BOOL || k == TY_CHAR || k == TY_SHORT ||
		k == TY_INT || k == TY_LONG || k == TY_ENUM
}

func copyType(ty *Type) *Type {
	ret := Type{}
	ret = *ty
	return &ret
}

func pointerTo(base *Type) *Type {
	ty := newType(TY_PTR, 8, 8)
	ty.base = base
	return ty
}

func arrayOf(base *Type, len int) *Type {
	ty := newType(TY_ARRAY, base.size*len, base.align)
	ty.base = base
	ty.arrayLen = len
	return ty
}

func enumType() *Type {
	return newType(TY_ENUM, 4, 4)
}

func getCommonType(ty1, ty2 *Type) *Type {
	if ty1.base != nil {
		return pointerTo(ty1.base)
	}
	if ty1.size == 8 || ty2.size == 8 {
		return tyLong
	}
	return tyInt
}

// This operation is called the "usual arithmetic conversion".
func usualArithConv(lhs, rhs **Node) {
	ty := getCommonType((*lhs).ty, (*rhs).ty)
	*lhs = NewCast(*lhs, ty)
	*rhs = NewCast(*rhs, ty)
}

func funcType(returnTy *Type) *Type {
	return &Type{kind: TY_FUNC, returnTy: returnTy}
}

func (node *Node) addType() {
	if node == nil || node.ty != nil {
		return
	}

	node.lhs.addType()
	node.rhs.addType()
	node.cond.addType()
	node.then.addType()
	node.els.addType()
	node.init.addType()
	node.inc.addType()

	for n := node.body; n != nil; n = n.next {
		n.addType()
	}
	for n := node.args; n != nil; n = n.next {
		n.addType()
	}

	switch node.kind {
	case ND_NUM:
		if node.val == int64(int32(node.val)) {
			node.ty = tyInt
		} else {
			node.ty = tyLong
		}
		return
	case ND_ADD, ND_SUB, ND_MUL, ND_DIV:
		usualArithConv(&node.lhs, &node.rhs)
		node.ty = node.lhs.ty
		return
	case ND_NEG:
		ty := getCommonType(tyInt, node.lhs.ty)
		node.lhs = NewCast(node.lhs, ty)
		node.ty = ty
		return
	case ND_ASSIGN:
		if node.lhs.ty.kind == TY_ARRAY {
			failTok(node.lhs.tok, "not an lvalue")
		}
		if node.lhs.ty.kind != TY_STRUCT {
			node.rhs = NewCast(node.rhs, node.lhs.ty)
		}
		node.ty = node.lhs.ty
		return
	case ND_EQ, ND_NE, ND_LT, ND_LE:
		usualArithConv(&node.lhs, &node.rhs)
		node.ty = tyInt
		return
	case ND_FUNCALL:
		node.ty = tyLong
		return
	case ND_VAR:
		node.ty = node.vara.ty
		return
	case ND_COMMA:
		node.ty = node.rhs.ty
		return
	case ND_MEMBER:
		node.ty = node.member.ty
		return
	case ND_ADDR:
		if node.lhs.ty.kind == TY_ARRAY {
			node.ty = pointerTo(node.lhs.ty.base)
		} else {
			node.ty = pointerTo(node.lhs.ty)
		}
		return
	case ND_DEREF:
		if node.lhs.ty.base == nil {
			failTok(node.tok, "invalid pointer dereference")
		}
		if node.lhs.ty.base.kind == TY_VOID {
			failTok(node.tok, "dereferencing a void pointer")
		}
		node.ty = node.lhs.ty.base
		return
	case ND_STMT_EXPR:
		if node.body != nil {
			stmt := node.body
			for stmt.next != nil {
				stmt = stmt.next
			}
			if stmt.kind == ND_EXPR_STMT {
				node.ty = stmt.lhs.ty
				return
			}
		}
		failTok(node.tok, "statement expression returning void is not supported")
		return
	}
}
