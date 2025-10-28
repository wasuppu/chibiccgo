package main

var tyInt = &Type{kind: TY_INT}

type TypeKind int

const (
	TY_INT TypeKind = iota
	TY_PTR
	TY_FUNC
)

type Type struct {
	kind TypeKind

	// Pointer
	base *Type

	// Declaration
	name *Token

	// Function type
	returnTy *Type
}

func (ty Type) isInteger() bool {
	return ty.kind == TY_INT
}

func pointerTo(base *Type) *Type {
	return &Type{
		kind: TY_PTR,
		base: base,
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

	switch node.kind {
	case ND_ADD, ND_SUB, ND_MUL, ND_DIV, ND_NEG, ND_ASSIGN:
		node.ty = node.lhs.ty
		return
	case ND_EQ, ND_NE, ND_LT, ND_LE, ND_NUM, ND_FUNCALL:
		node.ty = tyInt
		return
	case ND_VAR:
		node.ty = node.vara.ty
		return
	case ND_ADDR:
		node.ty = pointerTo(node.lhs.ty)
		return
	case ND_DEREF:
		if node.lhs.ty.kind != TY_PTR {
			failTok(node.tok, "invalid pointer dereference")
		}
		node.ty = node.lhs.ty.base
		return
	}
}
