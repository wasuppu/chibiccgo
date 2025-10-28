package main

// AST node
type NodeKind int

const (
	ND_ADD NodeKind = iota // +
	ND_SUB                 // -
	ND_MUL                 // *
	ND_DIV                 // /
	ND_NEG                 // unary -
	ND_NUM                 // Integer
)

// AST node type
type Node struct {
	kind NodeKind // Node kind
	lhs  *Node    // Left-hand side
	rhs  *Node    // Right-hand side
	val  int      // Used if kind == ND_NUM
}

// Create a new AST node.
func NewNode(kind NodeKind) *Node {
	return &Node{
		kind: kind,
	}
}

func NewBinary(kind NodeKind, lhs, rhs *Node) *Node {
	return &Node{
		kind: kind,
		lhs:  lhs,
		rhs:  rhs,
	}
}

func NewUnary(kind NodeKind, expr *Node) *Node {
	node := NewNode(kind)
	node.lhs = expr
	return node
}

func NewNum(val int) *Node {
	return &Node{
		kind: ND_NUM,
		val:  val,
	}
}

// expr = mul ("+" mul | "-" mul)*
func expr(rest **Token, tok *Token) *Node {
	node := mul(&tok, tok)

	for {
		if tok.equal("+") {
			node = NewBinary(ND_ADD, node, mul(&tok, tok.next))
			continue
		}

		if tok.equal("-") {
			node = NewBinary(ND_SUB, node, mul(&tok, tok.next))
			continue
		}

		*rest = tok
		return node
	}
}

// mul = unary ("*" unary | "/" unary)*
func mul(rest **Token, tok *Token) *Node {
	node := unary(&tok, tok)

	for {
		if tok.equal("*") {
			node = NewBinary(ND_MUL, node, unary(&tok, tok.next))
			continue
		}

		if tok.equal("/") {
			node = NewBinary(ND_DIV, node, unary(&tok, tok.next))
			continue
		}

		*rest = tok
		return node
	}
}

// unary = ("+" | "-") unary
// | primary
func unary(rest **Token, tok *Token) *Node {
	if tok.equal("+") {
		return unary(rest, tok.next)
	}

	if tok.equal("-") {
		return NewUnary(ND_NEG, unary(rest, tok.next))
	}

	return primary(rest, tok)
}

// primary = "(" expr ")" | num
func primary(rest **Token, tok *Token) *Node {
	if tok.equal("(") {
		node := expr(&tok, tok.next)
		*rest = tok.skip(")")
		return node
	}

	if tok.kind == TK_NUM {
		node := NewNum(tok.val)
		*rest = tok.next
		return node
	}

	failTok(tok, "expected an expression")
	return nil
}
