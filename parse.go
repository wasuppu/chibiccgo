package main

import "fmt"

// All local variable instances created during parsing are
// accumulated to this list.
var locals *Obj

// Likewise, global variables are accumulated to this list.
var globals *Obj
var scope = &Scope{}

// Struct member
type Member struct {
	next   *Member
	ty     *Type
	name   *Token
	offset int
}

// Variable or function
type Obj struct {
	next    *Obj
	name    string // Variable name
	ty      *Type  // Type
	isLocal bool   // local or global/function

	// Local variable
	offset int

	// Global variable or function
	isFunction   bool
	isDefinition bool

	// Global variable
	initData string

	// Function
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
	ND_COMMA                     // ,
	ND_MEMBER                    // . (struct member access)
	ND_ADDR                      // unary &
	ND_DEREF                     // unary *
	ND_RETURN                    // "return"
	ND_IF                        // "if"
	ND_FOR                       // "for"
	ND_BLOCK                     // { ... }
	ND_FUNCALL                   // Function call
	ND_EXPR_STMT                 // Expression statement
	ND_STMT_EXPR                 // Statement expression
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

	// Block or statement expression
	body *Node

	// Struct member access
	member *Member

	// Function call
	funcname string
	args     *Node

	vara *Obj  // Used if kind == ND_VAR
	val  int64 // Used if kind == ND_NUM
}

// Scope for local or global variables.
type VarScope struct {
	next *VarScope
	name string
	vara *Obj
}

// Scope for struct or union tags
type TagScope struct {
	next *TagScope
	name string
	ty   *Type
}

// Represents a block scope.
type Scope struct {
	next *Scope
	// C has two block scopes; one is for variables and the other is
	// for struct tags.
	vars *VarScope
	tags *TagScope
}

func enterScope() {
	sc := &Scope{next: scope}
	scope = sc
}

func leaveScope() {
	scope = scope.next
}

func pushScope(name string, vara *Obj) *VarScope {
	sc := &VarScope{name: name, vara: vara, next: scope.vars}
	scope.vars = sc
	return sc
}

func pushTagScope(tok *Token, ty *Type) {
	sc := &TagScope{
		name: tok.lexeme,
		ty:   ty,
		next: scope.tags,
	}
	scope.tags = sc
}

// Find a variable by name.
func findVar(tok *Token) *Obj {
	for sc := scope; sc != nil; sc = sc.next {
		for sc2 := sc.vars; sc2 != nil; sc2 = sc2.next {
			if tok.equal(sc2.name) {
				return sc2.vara
			}
		}
	}
	return nil
}

func findTag(tok *Token) *Type {
	for sc := scope; sc != nil; sc = sc.next {
		for sc2 := sc.tags; sc2 != nil; sc2 = sc2.next {
			if tok.equal(sc2.name) {
				return sc2.ty
			}
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

func NewNum(val int64, tok *Token) *Node {
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

func NewVar(name string, ty *Type) *Obj {
	vara := &Obj{
		name: name,
		ty:   ty,
	}
	pushScope(name, vara)
	return vara
}

func NewLVar(name string, ty *Type) *Obj {
	vara := NewVar(name, ty)
	vara.isLocal = true
	vara.next = locals
	locals = vara
	return vara
}

func NewGVar(name string, ty *Type) *Obj {
	vara := NewVar(name, ty)
	vara.next = globals
	globals = vara
	return vara
}

var id = 0

func newUniqueName() string {
	buf := fmt.Sprintf(".L..%d", id)
	id++
	return buf
}

func newAnonGVar(ty *Type) *Obj {
	return NewGVar(newUniqueName(), ty)
}

func newStringLiteral(p string, ty *Type) *Obj {
	vara := newAnonGVar(ty)
	vara.initData = p
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
	return int(tok.val)
}

// declspec = "char" | "short" | "int" | "long" | struct-decl | union-decl
func declspec(rest **Token, tok *Token) *Type {
	if tok.equal("char") {
		*rest = tok.next
		return tyChar
	}

	if tok.equal("short") {
		*rest = tok.next
		return tyShort
	}

	if tok.equal("int") {
		*rest = tok.next
		return tyInt
	}

	if tok.equal("long") {
		*rest = tok.next
		return tyLong
	}

	if tok.equal("struct") {
		return structDecl(rest, tok.next)
	}

	if tok.equal("union") {
		return unionDecl(rest, tok.next)
	}

	failTok(tok, "typename expected")
	return nil
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
// | "[" num "]" type-suffix
// | ε
func typeSuffix(rest **Token, tok *Token, ty *Type) *Type {
	if tok.equal("(") {
		return funcParams(rest, tok.next, ty)
	}

	if tok.equal("[") {
		sz := getNumber(tok.next)
		tok = tok.next.next.skip("]")
		ty = typeSuffix(rest, tok, ty)
		return arrayOf(ty, sz)
	}

	*rest = tok
	return ty
}

// declarator = "*"* ("(" ident ")" | "(" declarator ")" | ident) type-suffix
func declarator(rest **Token, tok *Token, ty *Type) *Type {
	for consume(&tok, tok, "*") {
		ty = pointerTo(ty)
	}

	if tok.equal("(") {
		start := tok
		dummy := Type{}
		declarator(&tok, start.next, &dummy)
		tok = tok.skip(")")
		ty = typeSuffix(rest, tok, ty)
		return declarator(&tok, start.next, ty)
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

// Returns true if a given token represents a type.
func isTypename(tok *Token) bool {
	return tok.equal("char") || tok.equal("short") || tok.equal("int") ||
		tok.equal("long") || tok.equal("struct") || tok.equal("union")
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

	enterScope()

	for !tok.equal("}") {
		if isTypename(tok) {
			cur.next = declaration(&tok, tok)
			cur = cur.next
		} else {
			cur.next = stmt(&tok, tok)
			cur = cur.next
		}
		cur.addType()
	}

	leaveScope()

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

// expr = assign ("," expr)?
func expr(rest **Token, tok *Token) *Node {
	node := assign(&tok, tok)

	if tok.equal(",") {
		return NewBinary(ND_COMMA, node, expr(rest, tok.next), tok)
	}

	*rest = tok
	return node
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
	rhs = NewBinary(ND_MUL, rhs, NewNum(int64(lhs.ty.base.size), tok), tok)
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
		rhs = NewBinary(ND_MUL, rhs, NewNum(int64(lhs.ty.base.size), tok), tok)
		rhs.addType()
		node := NewBinary(ND_SUB, lhs, rhs, tok)
		node.ty = lhs.ty
		return node
	}

	// ptr - ptr, which returns how many elements are between the two.
	if lhs.ty.base != nil && rhs.ty.base != nil {
		node := NewBinary(ND_SUB, lhs, rhs, tok)
		node.ty = tyInt
		return NewBinary(ND_DIV, node, NewNum(int64(lhs.ty.base.size), tok), tok)
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
// | postfix
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

	return postfix(rest, tok)
}

// struct-members = (declspec declarator (","  declarator)* ";")*
func structMembers(rest **Token, tok *Token, ty *Type) {
	head := Member{}
	cur := &head

	for !tok.equal("}") {
		basety := declspec(&tok, tok)
		i := 0

		for !consume(&tok, tok, ";") {
			if i != 0 {
				tok = tok.skip(",")
			}
			i++

			mem := &Member{}
			mem.ty = declarator(&tok, tok, basety)
			mem.name = mem.ty.name
			cur.next = mem
			cur = cur.next
		}
	}

	*rest = tok.next
	ty.members = head.next
}

// struct-union-decl = ident? ("{" struct-members)?
func structUnionDecl(rest **Token, tok *Token) *Type {
	// Read a tag.
	var tag *Token
	if tok.kind == TK_IDENT {
		tag = tok
		tok = tok.next
	}

	if tag != nil && !tok.equal("{") {
		ty := findTag(tag)
		if ty == nil {
			failTok(tag, "unknown struct type")
		}
		*rest = tok
		return ty
	}

	// Construct a struct object.
	ty := &Type{}
	ty.kind = TY_STRUCT
	structMembers(rest, tok.next, ty)
	ty.align = 1

	// Register the struct type if a name was given.
	if tag != nil {
		pushTagScope(tag, ty)
	}

	return ty
}

// struct-decl = struct-union-decl
func structDecl(rest **Token, tok *Token) *Type {
	ty := structUnionDecl(rest, tok)
	ty.kind = TY_STRUCT

	// Assign offsets within the struct to members.
	offset := 0
	for mem := ty.members; mem != nil; mem = mem.next {
		offset = alignTo(offset, mem.ty.align)
		mem.offset = offset
		offset += mem.ty.size

		if ty.align < mem.ty.align {
			ty.align = mem.ty.align
		}
	}
	ty.size = alignTo(offset, ty.align)
	return ty
}

// union-decl = struct-union-decl
func unionDecl(rest **Token, tok *Token) *Type {
	ty := structUnionDecl(rest, tok)
	ty.kind = TY_UNION

	// If union, we don't have to assign offsets because they
	// are already initialized to zero. We need to compute the
	// alignment and the size though.
	for mem := ty.members; mem != nil; mem = mem.next {
		if ty.align < mem.ty.align {
			ty.align = mem.ty.align
		}
		if ty.size < mem.ty.size {
			ty.size = mem.ty.size
		}
	}
	ty.size = alignTo(ty.size, ty.align)
	return ty
}

func getStructMember(ty *Type, tok *Token) *Member {
	for mem := ty.members; mem != nil; mem = mem.next {
		if mem.name.len == tok.len && mem.name.lexeme == tok.lexeme {
			return mem
		}
	}
	failTok(tok, "no such member")
	return nil
}

func structRef(lhs *Node, tok *Token) *Node {
	lhs.addType()
	if lhs.ty.kind != TY_STRUCT && lhs.ty.kind != TY_UNION {
		failTok(lhs.tok, "not a struct nor a union")
	}

	node := NewUnary(ND_MEMBER, lhs, tok)
	node.member = getStructMember(lhs.ty, tok)
	return node
}

// postfix = primary ("[" expr "]" | "." ident | "->" ident)*
func postfix(rest **Token, tok *Token) *Node {
	node := primary(&tok, tok)

	for {
		if tok.equal("[") {
			// x[y] is short for *(x+y)
			start := tok
			idx := expr(&tok, tok.next)
			tok = tok.skip("]")
			node = NewUnary(ND_DEREF, newAdd(node, idx, start), start)
			continue
		}

		if tok.equal(".") {
			node = structRef(node, tok.next)
			tok = tok.next.next
			continue
		}

		if tok.equal("->") {
			// x->y is short for (*x).y
			node = NewUnary(ND_DEREF, node, tok)
			node = structRef(node, tok.next)
			tok = tok.next.next
			continue
		}

		*rest = tok
		return node
	}
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

// primary = "(" "{" stmt+ "}" ")"
// | "(" expr ")"
// | "sizeof" unary
// | ident func-args?
// | str
// | num
func primary(rest **Token, tok *Token) *Node {
	if tok.equal("(") && tok.next.equal("{") {
		// This is a GNU statement expresssion.
		node := NewNode(ND_STMT_EXPR, tok)
		node.body = compoundStmt(&tok, tok.next.next).body
		*rest = tok.skip(")")
		return node
	}

	if tok.equal("(") {
		node := expr(&tok, tok.next)
		*rest = tok.skip(")")
		return node
	}

	if tok.equal("sizeof") {
		node := unary(rest, tok.next)
		node.addType()
		return NewNum(int64(node.ty.size), tok)
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

	if tok.kind == TK_STR {
		vara := newStringLiteral(tok.str, tok.ty)
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

func function(tok *Token, basety *Type) *Token {
	ty := declarator(&tok, tok, basety)

	fn := NewGVar(getIdent(ty.name), ty)
	fn.isFunction = true
	fn.isDefinition = !consume(&tok, tok, ";")

	if !fn.isDefinition {
		return tok
	}

	locals = nil
	enterScope()
	createParamLvars(ty.params)
	fn.params = locals

	tok = tok.skip("{")
	fn.body = compoundStmt(&tok, tok)
	fn.locals = locals
	leaveScope()
	return tok
}

func globalVariable(tok *Token, basety *Type) *Token {
	first := true

	for !consume(&tok, tok, ";") {
		if !first {
			tok = tok.skip(",")
		}
		first = false

		ty := declarator(&tok, tok, basety)
		NewGVar(getIdent(ty.name), ty)
	}
	return tok
}

// Lookahead tokens and returns true if a given token is a start
// of a function definition or declaration.
func isFunction(tok *Token) bool {
	if tok.equal(";") {
		return false
	}

	dummy := Type{}
	ty := declarator(&tok, tok, &dummy)
	return ty.kind == TY_FUNC
}

// program = (function-definition | global-variable)*
func parse(tok *Token) *Obj {
	globals = nil

	for tok.kind != TK_EOF {
		basety := declspec(&tok, tok)

		// Function
		if isFunction(tok) {
			tok = function(tok, basety)
			continue
		}

		// Global variable
		tok = globalVariable(tok, basety)
	}

	return globals
}
