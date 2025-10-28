package main

// All local variable instances created during parsing are
// accumulated to this list.
var locals *Obj

// Local variable
type Obj struct {
	next   *Obj
	name   string // Variable name
	ty     *Type  // Type
	offset int    // Offset from RBP
}

// Function
type Function struct {
	next      *Function
	name      string
	params    *Obj
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
	ND_ADDR                      // unary &
	ND_DEREF                     // unary *
	ND_RETURN                    // "return"
	ND_IF                        // "if"
	ND_FOR                       // "for"
	ND_BLOCK                     // { ... }
	ND_FUNCALL                   // Function call
	ND_EXPR_STMT                 // Expression statement
	ND_VAR                       // Variable
	ND_NUM                       // Integer
)

// AST node type
type Node struct {
	kind NodeKind // Node kind
	next *Node    // Next node
	ty   *Type    // Type, e.g. int or pointer to int
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

	// Function call
	funcname string
	args     *Node

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

func NewLVar(name string, ty *Type) *Obj {
	vara := &Obj{name: name, ty: ty, next: locals}
	locals = vara
	return vara
}

func getIdent(tok *Token) string {
	if tok.kind != TK_IDENT {
		failTok(tok, "expected an identifier")
	}
	return tok.lexeme
}

func getNumber(tok *Token) int {
	if tok.kind != TK_NUM {
		failTok(tok, "expected a number")
	}
	return tok.val
}

// declspec = "int"
func declspec(rest **Token, tok *Token) *Type {
	*rest = tok.skip("int")
	return tyInt
}

// func-params = (param ("," param)*)? ")"
// param       = declspec declarator
func funcParams(rest **Token, tok *Token, ty *Type) *Type {
	head := Type{}
	cur := &head

	for !tok.equal(")") {
		if cur != &head {
			tok = tok.skip(",")
		}
		basety := declspec(&tok, tok)
		ty := declarator(&tok, tok, basety)
		cur.next = copyType(ty)
		cur = cur.next
	}

	ty = funcType(ty)
	ty.params = head.next
	*rest = tok.next
	return ty
}

// type-suffix = "(" func-params
// | "[" num "]"
// | ε
func typeSuffix(rest **Token, tok *Token, ty *Type) *Type {
	if tok.equal("(") {
		return funcParams(rest, tok.next, ty)
	}

	if tok.equal("[") {
		sz := getNumber(tok.next)
		*rest = tok.next.next.skip("]")
		return arrayOf(ty, sz)
	}

	*rest = tok
	return ty
}

// declarator = "*"* ident type-suffix
func declarator(rest **Token, tok *Token, ty *Type) *Type {
	for consume(&tok, tok, "*") {
		ty = pointerTo(ty)
	}

	if tok.kind != TK_IDENT {
		failTok(tok, "expected a variable name")
	}

	ty = typeSuffix(rest, tok.next, ty)
	ty.name = tok
	return ty
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
func declaration(rest **Token, tok *Token) *Node {
	basety := declspec(&tok, tok)

	head := Node{}
	cur := &head
	i := 0

	for !tok.equal(";") {
		if i > 0 {
			tok = tok.skip(",")
		}
		i++

		ty := declarator(&tok, tok, basety)
		vara := NewLVar(getIdent(ty.name), ty)

		if !tok.equal("=") {
			continue
		}

		lhs := NewVarNode(vara, ty.name)
		rhs := assign(&tok, tok.next)
		node := NewBinary(ND_ASSIGN, lhs, rhs, tok)
		cur.next = NewUnary(ND_EXPR_STMT, node, tok)
		cur = cur.next
	}

	node := NewNode(ND_BLOCK, tok)
	node.body = head.next
	*rest = tok.next
	return node
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

// compound-stmt = (declaration | stmt)* "}"
func compoundStmt(rest **Token, tok *Token) *Node {
	node := NewNode(ND_BLOCK, tok)

	head := Node{}
	cur := &head
	for !tok.equal("}") {
		if tok.equal("int") {
			cur.next = declaration(&tok, tok)
			cur = cur.next
		} else {
			cur.next = stmt(&tok, tok)
			cur = cur.next
		}
		cur.addType()
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

// In C, `+` operator is overloaded to perform the pointer arithmetic.
// If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
// so that p+n points to the location n elements (not bytes) ahead of p.
// In other words, we need to scale an integer value before adding to a
// pointer value. This function takes care of the scaling.
func newAdd(lhs, rhs *Node, tok *Token) *Node {
	lhs.addType()
	rhs.addType()

	// num + num
	if lhs.ty.isInteger() && rhs.ty.isInteger() {
		return NewBinary(ND_ADD, lhs, rhs, tok)
	}

	if lhs.ty.base != nil && rhs.ty.base != nil {
		failTok(tok, "invalid operands")
	}

	// Canonicalize `num + ptr` to `ptr + num`.
	if lhs.ty.base == nil && rhs.ty.base != nil {
		lhs, rhs = rhs, lhs
	}

	// ptr + num
	rhs = NewBinary(ND_MUL, rhs, NewNum(lhs.ty.base.size, tok), tok)
	return NewBinary(ND_ADD, lhs, rhs, tok)
}

// Like `+`, `-` is overloaded for the pointer type.
func newSub(lhs, rhs *Node, tok *Token) *Node {
	lhs.addType()
	rhs.addType()

	// num - num
	if lhs.ty.isInteger() && rhs.ty.isInteger() {
		return NewBinary(ND_SUB, lhs, rhs, tok)
	}

	// ptr - num
	if lhs.ty.base != nil && rhs.ty.isInteger() {
		rhs = NewBinary(ND_MUL, rhs, NewNum(lhs.ty.base.size, tok), tok)
		rhs.addType()
		node := NewBinary(ND_SUB, lhs, rhs, tok)
		node.ty = lhs.ty
		return node
	}

	// ptr - ptr, which returns how many elements are between the two.
	if lhs.ty.base != nil && rhs.ty.base != nil {
		node := NewBinary(ND_SUB, lhs, rhs, tok)
		node.ty = tyInt
		return NewBinary(ND_DIV, node, NewNum(lhs.ty.base.size, tok), tok)
	}

	failTok(tok, "invalid operands")
	return nil
}

// add = mul ("+" mul | "-" mul)*
func add(rest **Token, tok *Token) *Node {
	node := mul(&tok, tok)

	for {
		start := tok

		if tok.equal("+") {
			node = newAdd(node, mul(&tok, tok.next), start)
			continue
		}

		if tok.equal("-") {
			node = newSub(node, mul(&tok, tok.next), start)
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

// unary = ("+" | "-" | "*" | "&") unary
// | primary
func unary(rest **Token, tok *Token) *Node {
	if tok.equal("+") {
		return unary(rest, tok.next)
	}

	if tok.equal("-") {
		return NewUnary(ND_NEG, unary(rest, tok.next), tok)
	}

	if tok.equal("&") {
		return NewUnary(ND_ADDR, unary(rest, tok.next), tok)
	}

	if tok.equal("*") {
		return NewUnary(ND_DEREF, unary(rest, tok.next), tok)
	}

	return primary(rest, tok)
}

// funcall = ident "(" (assign ("," assign)*)? ")"
func funcall(rest **Token, tok *Token) *Node {
	start := tok
	tok = tok.next.next

	head := Node{}
	cur := &head

	for !tok.equal(")") {
		if cur != &head {
			tok = tok.skip(",")
		}
		cur.next = assign(&tok, tok)
		cur = cur.next
	}

	*rest = tok.skip(")")

	node := NewNode(ND_FUNCALL, start)
	node.funcname = start.lexeme
	node.args = head.next
	return node
}

// primary = "(" expr ")" | ident func-args? | num
func primary(rest **Token, tok *Token) *Node {
	if tok.equal("(") {
		node := expr(&tok, tok.next)
		*rest = tok.skip(")")
		return node
	}

	if tok.kind == TK_IDENT {
		// Function call
		if tok.next.equal("(") {
			return funcall(rest, tok)
		}

		// Variable
		vara := findVar(tok)
		if vara == nil {
			failTok(tok, "undefined variable")
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

func createParamLvars(param *Type) {
	if param != nil {
		createParamLvars(param.next)
		NewLVar(getIdent(param.name), param)
	}
}

func function(rest **Token, tok *Token) *Function {
	ty := declspec(&tok, tok)
	ty = declarator(&tok, tok, ty)

	locals = nil

	fn := &Function{name: getIdent(ty.name)}
	createParamLvars(ty.params)
	fn.params = locals

	tok = tok.skip("{")
	fn.body = compoundStmt(rest, tok)
	fn.locals = locals
	return fn
}

// program = function-definition*
func parse(tok *Token) *Function {
	head := Function{}
	cur := &head

	for tok.kind != TK_EOF {
		cur.next = function(&tok, tok)
		cur = cur.next
	}

	return head.next
}
