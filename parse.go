package main

// All local variable instances created during parsing are
// accumulated to this list.
var locals *Obj

// Local variable
type Obj struct {
	next   *Obj
	name   string // Variable name
	offset int    // Offset from RBP
}

// Function
type Function struct {
	body      *Node
	locals    *Obj
	stackSize int
}

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
	ND_RETURN                    // "return"
	ND_IF                        // "if"
	ND_FOR                       // "for"
	ND_BLOCK                     // { ... }
	ND_EXPR_STMT                 // Expression statement
	ND_VAR                       // Variable
	ND_NUM                       // Integer
)

// AST node type
type Node struct {
	kind NodeKind // Node kind
	next *Node    // Next node
	tok  *Token   // Representative token

	lhs *Node // Left-hand side
	rhs *Node // Right-hand side

	// "if" or "for" statement
	cond *Node
	then *Node
	els  *Node
	init *Node
	inc  *Node

	// Block
	body *Node

	vara *Obj // Used if kind == ND_VAR
	val  int  // Used if kind == ND_NUM
}

// Find a local variable by name.
func findVar(tok *Token) *Obj {
	for vara := locals; vara != nil; vara = vara.next {
		if len(vara.name) == tok.len && tok.lexeme == vara.name {
			return vara
		}
	}
	return nil
}

// Create a new AST node.
func NewNode(kind NodeKind, tok *Token) *Node {
	return &Node{
		kind: kind,
		tok:  tok,
	}
}

func NewBinary(kind NodeKind, lhs, rhs *Node, tok *Token) *Node {
	return &Node{
		kind: kind,
		tok:  tok,
		lhs:  lhs,
		rhs:  rhs,
	}
}

func NewUnary(kind NodeKind, expr *Node, tok *Token) *Node {
	node := NewNode(kind, tok)
	node.lhs = expr
	return node
}

func NewNum(val int, tok *Token) *Node {
	return &Node{
		kind: ND_NUM,
		tok:  tok,
		val:  val,
	}
}

func NewVarNode(vara *Obj, tok *Token) *Node {
	node := NewNode(ND_VAR, tok)
	node.vara = vara
	return node
}

func NewLVar(name string) *Obj {
	vara := &Obj{name: name, next: locals}
	locals = vara
	return vara
}

// stmt = "return" expr ";"
// | "if" "(" expr ")" stmt ("else" stmt)?
// | "for" "(" expr-stmt expr? ";" expr? ")" stmt
// | "while" "(" expr ")" stmt
// | "{" compound-stmt
// | expr-stmt
func stmt(rest **Token, tok *Token) *Node {
	if tok.equal("return") {
		node := NewNode(ND_RETURN, tok)
		node.lhs = expr(&tok, tok.next)
		*rest = tok.skip(";")
		return node
	}

	if tok.equal("if") {
		node := NewNode(ND_IF, tok)
		tok = tok.next.skip("(")
		node.cond = expr(&tok, tok)
		tok = tok.skip(")")
		node.then = stmt(&tok, tok)
		if tok.equal("else") {
			node.els = stmt(&tok, tok.next)
		}
		*rest = tok
		return node
	}

	if tok.equal("for") {
		node := NewNode(ND_FOR, tok)
		tok = tok.next.skip("(")

		node.init = exprStmt(&tok, tok)

		if !tok.equal(";") {
			node.cond = expr(&tok, tok)
		}
		tok = tok.skip(";")

		if !tok.equal(")") {
			node.inc = expr(&tok, tok)
		}
		tok = tok.skip(")")

		node.then = stmt(rest, tok)
		return node
	}

	if tok.equal("while") {
		node := NewNode(ND_FOR, tok)
		tok = tok.next.skip("(")
		node.cond = expr(&tok, tok)
		tok = tok.skip(")")
		node.then = stmt(rest, tok)
		return node
	}

	if tok.equal("{") {
		return compoundStmt(rest, tok.next)
	}

	return exprStmt(rest, tok)
}

// compound-stmt = stmt* "}"
func compoundStmt(rest **Token, tok *Token) *Node {
	node := NewNode(ND_BLOCK, tok)

	head := Node{}
	cur := &head
	for !tok.equal("}") {
		cur.next = stmt(&tok, tok)
		cur = cur.next
	}

	node.body = head.next
	*rest = tok.next
	return node
}

// expr-stmt = expr? ";"
func exprStmt(rest **Token, tok *Token) *Node {
	if tok.equal(";") {
		*rest = tok.next
		return NewNode(ND_BLOCK, tok)
	}

	node := NewNode(ND_EXPR_STMT, tok)
	node.lhs = expr(&tok, tok)
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
		return NewBinary(ND_ASSIGN, node, assign(rest, tok.next), tok)
	}

	*rest = tok
	return node
}

// equality = relational ("==" relational | "!=" relational)*
func equality(rest **Token, tok *Token) *Node {
	node := relational(&tok, tok)

	for {
		start := tok

		if tok.equal("==") {
			node = NewBinary(ND_EQ, node, relational(&tok, tok.next), start)
			continue
		}

		if tok.equal("!=") {
			node = NewBinary(ND_NE, node, relational(&tok, tok.next), start)
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
		start := tok

		if tok.equal("<") {
			node = NewBinary(ND_LT, node, add(&tok, tok.next), start)
			continue
		}

		if tok.equal("<=") {
			node = NewBinary(ND_LE, node, add(&tok, tok.next), start)
			continue
		}

		if tok.equal(">") {
			node = NewBinary(ND_LT, add(&tok, tok.next), node, start)
			continue
		}

		if tok.equal(">=") {
			node = NewBinary(ND_LE, add(&tok, tok.next), node, start)
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
		start := tok

		if tok.equal("+") {
			node = NewBinary(ND_ADD, node, mul(&tok, tok.next), start)
			continue
		}

		if tok.equal("-") {
			node = NewBinary(ND_SUB, node, mul(&tok, tok.next), start)
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
		start := tok

		if tok.equal("*") {
			node = NewBinary(ND_MUL, node, unary(&tok, tok.next), start)
			continue
		}

		if tok.equal("/") {
			node = NewBinary(ND_DIV, node, unary(&tok, tok.next), start)
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
		return NewUnary(ND_NEG, unary(rest, tok.next), tok)
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
		vara := findVar(tok)
		if vara == nil {
			vara = NewLVar(tok.lexeme)
		}
		*rest = tok.next
		return NewVarNode(vara, tok)
	}

	if tok.kind == TK_NUM {
		node := NewNum(tok.val, tok)
		*rest = tok.next
		return node
	}

	failTok(tok, "expected an expression")
	return nil
}

// program = stmt*
func parse(tok *Token) *Function {
	tok = tok.skip("{")

	prog := &Function{}
	prog.body = compoundStmt(&tok, tok)
	prog.locals = locals
	return prog
}
