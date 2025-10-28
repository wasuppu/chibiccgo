package main

var tyChar = &Type{kind: TY_CHAR, size: 1}
var tyInt = &Type{kind: TY_INT, size: 8}

type TypeKind int

const (
	TY_CHAR TypeKind = iota
	TY_INT
	TY_PTR
	TY_FUNC
	TY_ARRAY
	TY_STRUCT
)

type Type struct {
	kind TypeKind
	size int // sizeof() value

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

func (ty Type) isInteger() bool {
	return ty.kind == TY_CHAR || ty.kind == TY_INT
}

func copyType(ty *Type) *Type {
	ret := Type{}
	ret = *ty
	return &ret
}

func pointerTo(base *Type) *Type {
	return &Type{
		kind: TY_PTR,
		size: 8,
		base: base,
	}
}

func arrayOf(base *Type, len int) *Type {
	return &Type{
		kind:     TY_ARRAY,
		size:     base.size * len,
		base:     base,
		arrayLen: len,
	}
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
	case ND_ADD, ND_SUB, ND_MUL, ND_DIV, ND_NEG:
		node.ty = node.lhs.ty
		return
	case ND_ASSIGN:
		if node.lhs.ty.kind == TY_ARRAY {
			failTok(node.lhs.tok, "not an lvalue")
		}
		node.ty = node.lhs.ty
		return
	case ND_EQ, ND_NE, ND_LT, ND_LE, ND_NUM, ND_FUNCALL:
		node.ty = tyInt
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
