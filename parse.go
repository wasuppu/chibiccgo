package main

// AST node
type NodeKind int

const (
	ND_ADD       NodeKind = iota // +
	ND_SUB                       // -
	ND_MUL                       // *
	ND_DIV                       // /
	ND_NEG                       // unary -
	ND_EQ                        // ==
	ND_NE                        // !=
	ND_LT                        // <
	ND_LE                        // <=
	ND_ASSIGN                    // =
	ND_EXPR_STMT                 // Expression statement
	ND_VAR                       // Variable
	ND_NUM                       // Integer
)

// AST node type
type Node struct {
	kind NodeKind // Node kind
	next *Node    // Next node
	lhs  *Node    // Left-hand side
	rhs  *Node    // Right-hand side
	name byte     // Used if kind == ND_VAR
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

func NewVarNode(name byte) *Node {
	node := NewNode(ND_VAR)
	node.name = name
	return node
}

// stmt = expr-stmt
func stmt(rest **Token, tok *Token) *Node {
	return exprStmt(rest, tok)
}

// expr-stmt = expr ";"
func exprStmt(rest **Token, tok *Token) *Node {
	node := NewUnary(ND_EXPR_STMT, expr(&tok, tok))
	*rest = tok.skip(";")
	return node
}

// expr = assign
func expr(rest **Token, tok *Token) *Node {
	return assign(rest, tok)
}

// assign = equality ("=" assign)?
func assign(rest **Token, tok *Token) *Node {
	node := equality(&tok, tok)
	if tok.equal("=") {
		node = NewBinary(ND_ASSIGN, node, assign(&tok, tok.next))
	}
	*rest = tok
	return node
}

// equality = relational ("==" relational | "!=" relational)*
func equality(rest **Token, tok *Token) *Node {
	node := relational(&tok, tok)

	for {
		if tok.equal("==") {
			node = NewBinary(ND_EQ, node, relational(&tok, tok.next))
			continue
		}

		if tok.equal("!=") {
			node = NewBinary(ND_NE, node, relational(&tok, tok.next))
			continue
		}

		*rest = tok
		return node
	}
}

// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
func relational(rest **Token, tok *Token) *Node {
	node := add(&tok, tok)

	for {
		if tok.equal("<") {
			node = NewBinary(ND_LT, node, add(&tok, tok.next))
			continue
		}

		if tok.equal("<=") {
			node = NewBinary(ND_LE, node, add(&tok, tok.next))
			continue
		}

		if tok.equal(">") {
			node = NewBinary(ND_LT, add(&tok, tok.next), node)
			continue
		}

		if tok.equal(">=") {
			node = NewBinary(ND_LE, add(&tok, tok.next), node)
			continue
		}

		*rest = tok
		return node
	}
}

// add = mul ("+" mul | "-" mul)*
func add(rest **Token, tok *Token) *Node {
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

// primary = "(" expr ")" | ident | num
func primary(rest **Token, tok *Token) *Node {
	if tok.equal("(") {
		node := expr(&tok, tok.next)
		*rest = tok.skip(")")
		return node
	}

	if tok.kind == TK_IDENT {
		node := NewVarNode(source[tok.loc])
		*rest = tok.next
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

// program = stmt*
func parse(tok *Token) *Node {
	head := Node{}
	cur := &head

	for tok.kind != TK_EOF {
		cur.next = stmt(&tok, tok)
		cur = cur.next
	}

	return head.next
}
