package main

import "fmt"

// All local variable instances created during parsing are
// accumulated to this list.
var locals *Obj

// Likewise, global variables are accumulated to this list.
var globals *Obj
var scope = &Scope{}

// Points to the function object the parser is currently parsing.
var currentParseFn *Obj

// Lists of all goto statements and labels in the curent function.
var gotos *Node
var labels *Node

// Current "goto" and "continue" jump targets.
var brkLabel string
var contLabel string

// Points to a node representing a switch if we are parsing
// a switch statement. Otherwise, NULL.
var currentSwitch *Node

// This struct represents a variable initializer. Since initializers
// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
// is a tree data structure.
type Initializer struct {
	next *Initializer
	ty   *Type
	tok  *Token

	// If it's not an aggregate type and has an initializer,
	// `expr` has an initialization expression.
	expr *Node

	// If it's an initializer for an aggregate type (e.g. array or struct),
	// `children` has initializers for its children.
	children []*Initializer
}

// For local variable initializer.
type InitDesg struct {
	next *InitDesg
	idx  int
	vara *Obj
}

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
	isStatic     bool

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
	ND_NULL_EXPR NodeKind = iota // Do nothing
	ND_ADD                       // +
	ND_SUB                       // -
	ND_MUL                       // *
	ND_DIV                       // /
	ND_NEG                       // unary -
	ND_MOD                       // %
	ND_BITAND                    // &
	ND_BITOR                     // |
	ND_BITXOR                    // ^
	ND_SHL                       // <<
	ND_SHR                       // >>
	ND_EQ                        // ==
	ND_NE                        // !=
	ND_LT                        // <
	ND_LE                        // <=
	ND_ASSIGN                    // =
	ND_COND                      // ?:
	ND_COMMA                     // ,
	ND_MEMBER                    // . (struct member access)
	ND_ADDR                      // unary &
	ND_DEREF                     // unary *
	ND_NOT                       // !
	ND_BITNOT                    // ~
	ND_LOGAND                    // &&
	ND_LOGOR                     // ||
	ND_RETURN                    // "return"
	ND_IF                        // "if"
	ND_FOR                       // "for"
	ND_SWITCH                    // "switch"
	ND_CASE                      // "case"
	ND_BLOCK                     // { ... }
	ND_GOTO                      // "goto"
	ND_LABEL                     // Labeled statement
	ND_FUNCALL                   // Function call
	ND_EXPR_STMT                 // Expression statement
	ND_STMT_EXPR                 // Statement expression
	ND_VAR                       // Variable
	ND_NUM                       // Integer
	ND_CAST                      // Type cast
	ND_MEMZERO                   // Zero-clear a stack variable
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

	// "break" and "continue" labels
	brkLabel  string
	contLabel string

	// Block or statement expression
	body *Node

	// Struct member access
	member *Member

	// Function call
	funcname string
	functy   *Type
	args     *Node

	// Goto or labeled statement
	label       string
	uniqueLabel string
	gotoNext    *Node

	// Switch-cases
	caseNext    *Node
	defaultCase *Node

	// Variable
	vara *Obj

	// Numeric literal
	val int64
}

// Scope for local variables, global variables, typedefs
// or enum constants
type VarScope struct {
	next    *VarScope
	name    string
	vara    *Obj
	typedef *Type
	enumty  *Type
	enumval int
}

// Scope for struct, union or enum tags
type TagScope struct {
	next *TagScope
	name string
	ty   *Type
}

// Variable attributes such as typedef or extern.
type VarAttr struct {
	isTypedef bool
	isStatic  bool
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

func pushScope(name string) *VarScope {
	sc := &VarScope{name: name, next: scope.vars}
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
func findVar(tok *Token) *VarScope {
	for sc := scope; sc != nil; sc = sc.next {
		for sc2 := sc.vars; sc2 != nil; sc2 = sc2.next {
			if tok.equal(sc2.name) {
				return sc2
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

func NewLong(val int64, tok *Token) *Node {
	node := NewNode(ND_NUM, tok)
	node.val = val
	node.ty = tyLong
	return node
}

func NewVarNode(vara *Obj, tok *Token) *Node {
	node := NewNode(ND_VAR, tok)
	node.vara = vara
	return node
}

func NewCast(expr *Node, ty *Type) *Node {
	expr.addType()

	return &Node{
		kind: ND_CAST,
		tok:  expr.tok,
		lhs:  expr,
		ty:   copyType(ty),
	}
}

func NewInitializer(ty *Type) *Initializer {
	init := &Initializer{}
	init.ty = ty

	if ty.kind == TY_ARRAY {
		init.children = make([]*Initializer, ty.arrayLen)
		for i := range ty.arrayLen {
			init.children[i] = NewInitializer(ty.base)
		}
	}

	return init
}

func NewVar(name string, ty *Type) *Obj {
	vara := &Obj{
		name: name,
		ty:   ty,
	}
	pushScope(name).vara = vara
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

func findTypedef(tok *Token) *Type {
	if tok.kind == TK_IDENT {
		sc := findVar(tok)
		if sc != nil {
			return sc.typedef
		}
	}
	return nil
}

func getIdent(tok *Token) string {
	if tok.kind != TK_IDENT {
		failTok(tok, "expected an identifier")
	}
	return tok.lexeme
}

const (
	VOID  = 1 << 0
	BOOL  = 1 << 2
	CHAR  = 1 << 4
	SHORT = 1 << 6
	INT   = 1 << 8
	LONG  = 1 << 10
	OTHER = 1 << 12
)

// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
// | "typedef" | "static"
// | struct-decl | union-decl | typedef-name
// | enum-specifier)+
func declspec(rest **Token, tok *Token, attr *VarAttr) *Type {
	ty := tyInt
	counter := 0

	for isTypename(tok) {
		// Handle storage class specifiers.
		if tok.equal("typedef") || tok.equal("static") {
			if attr == nil {
				failTok(tok, "storage class specifier is not allowed in this context")
			}

			if tok.equal("typedef") {
				attr.isTypedef = true
			} else {
				attr.isStatic = true
			}

			if attr.isTypedef && attr.isStatic {
				failTok(tok, "typedef and static may not be used together")
			}
			tok = tok.next
			continue
		}

		// Handle user-defined types.
		ty2 := findTypedef(tok)
		if tok.equal("struct") || tok.equal("union") || tok.equal("enum") || ty2 != nil {
			if counter != 0 {
				break
			}

			if tok.equal("struct") {
				ty = structDecl(&tok, tok.next)
			} else if tok.equal("union") {
				ty = unionDecl(&tok, tok.next)
			} else if tok.equal("enum") {
				ty = enumSpecifier(&tok, tok.next)
			} else {
				ty = ty2
				tok = tok.next
			}

			counter += OTHER
			continue
		}

		// Handle built-in types.
		if tok.equal("void") {
			counter += VOID
		} else if tok.equal("_Bool") {
			counter += BOOL
		} else if tok.equal("char") {
			counter += CHAR
		} else if tok.equal("short") {
			counter += SHORT
		} else if tok.equal("int") {
			counter += INT
		} else if tok.equal("long") {
			counter += LONG
		} else {
			unreachable()
		}

		switch counter {
		case VOID:
			ty = tyVoid
		case BOOL:
			ty = tyBool
		case CHAR:
			ty = tyChar
		case SHORT, SHORT + INT:
			ty = tyShort
		case INT:
			ty = tyInt
		case LONG, LONG + INT,
			LONG + LONG,
			LONG + LONG + INT:
			ty = tyLong
		default:
			failTok(tok, "invalid type")
		}

		tok = tok.next
	}

	*rest = tok
	return ty
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

		ty2 := declspec(&tok, tok, nil)
		ty2 = declarator(&tok, tok, ty2)

		// "array of T" is converted to "pointer to T" only in the parameter
		// context. For example, *argv[] is converted to **argv by this.
		if ty2.kind == TY_ARRAY {
			name := ty2.name
			ty2 = pointerTo(ty2.base)
			ty2.name = name
		}

		cur.next = copyType(ty2)
		cur = cur.next
	}

	ty = funcType(ty)
	ty.params = head.next
	*rest = tok.next
	return ty
}

// array-dimensions = const-expr? "]" type-suffix
func arrayDimensions(rest **Token, tok *Token, ty *Type) *Type {
	if tok.equal("]") {
		ty = typeSuffix(rest, tok.next, ty)
		return arrayOf(ty, -1)
	}

	sz := int32(constExpr(&tok, tok))
	tok = tok.skip("]")
	ty = typeSuffix(rest, tok, ty)
	return arrayOf(ty, int(sz))
}

// type-suffix = "(" func-params
// | "[" array-dimensions
// | ε
func typeSuffix(rest **Token, tok *Token, ty *Type) *Type {
	if tok.equal("(") {
		return funcParams(rest, tok.next, ty)
	}

	if tok.equal("[") {
		return arrayDimensions(rest, tok.next, ty)
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

// abstract-declarator = "*"* ("(" abstract-declarator ")")? type-suffix
func abstractDeclarator(rest **Token, tok *Token, ty *Type) *Type {
	for tok.equal("*") {
		ty = pointerTo(ty)
		tok = tok.next
	}

	if tok.equal("(") {
		start := tok
		dummy := Type{}
		abstractDeclarator(&tok, start.next, &dummy)
		tok = tok.skip(")")
		ty = typeSuffix(rest, tok, ty)
		return abstractDeclarator(&tok, start.next, ty)
	}

	return typeSuffix(rest, tok, ty)
}

// type-name = declspec abstract-declarator
func typename(rest **Token, tok *Token) *Type {
	ty := declspec(&tok, tok, nil)
	return abstractDeclarator(rest, tok, ty)
}

// enum-specifier = ident? "{" enum-list? "}"
// | ident ("{" enum-list? "}")?
// enum-list      = ident ("=" num)? ("," ident ("=" num)?)*
func enumSpecifier(rest **Token, tok *Token) *Type {
	ty := enumType()

	// Read a struct tag.
	var tag *Token
	if tok.kind == TK_IDENT {
		tag = tok
		tok = tok.next
	}

	if tag != nil && !tok.equal("{") {
		ty = findTag(tag)
		if ty == nil {
			failTok(tag, "unknown enum type")
		}
		if ty.kind != TY_ENUM {
			failTok(tag, "not an enum tag")
		}
		*rest = tok
		return ty
	}

	tok = tok.skip("{")

	// Read an enum-list.
	i := 0
	val := 0
	for !tok.equal("}") {
		if i > 0 {
			tok = tok.skip(",")
		}
		i++

		name := getIdent(tok)
		tok = tok.next

		if tok.equal("=") {
			val = int(int32(constExpr(&tok, tok.next)))
		}

		sc := pushScope(name)
		sc.enumty = ty
		sc.enumval = val
		val++
	}

	*rest = tok.next

	if tag != nil {
		pushTagScope(tag, ty)
	}
	return ty
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
func declaration(rest **Token, tok *Token, basety *Type) *Node {
	head := Node{}
	cur := &head
	i := 0

	for !tok.equal(";") {
		if i > 0 {
			tok = tok.skip(",")
		}
		i++

		ty := declarator(&tok, tok, basety)
		if ty.size < 0 {
			failTok(tok, "variable has incomplete type")
		}
		if ty.kind == TY_VOID {
			failTok(tok, "variable declared void")
		}

		vara := NewLVar(getIdent(ty.name), ty)

		if tok.equal("=") {
			expr := lvarInitializer(&tok, tok.next, vara)
			cur.next = NewUnary(ND_EXPR_STMT, expr, tok)
			cur = cur.next
		}
	}

	node := NewNode(ND_BLOCK, tok)
	node.body = head.next
	*rest = tok.next
	return node
}

func skipExcessElement(tok *Token) *Token {
	if tok.equal("{") {
		tok = skipExcessElement(tok.next)
		return tok.skip("}")
	}

	assign(&tok, tok)
	return tok
}

// initializer = "{" initializer ("," initializer)* "}"
// | assign
func initializer2(rest **Token, tok *Token, init *Initializer) {
	if init.ty.kind == TY_ARRAY {
		tok = tok.skip("{")

		for i := 0; !consume(rest, tok, "}"); i++ {
			if i > 0 {
				tok = tok.skip(",")
			}

			if i < init.ty.arrayLen {
				initializer2(&tok, tok, init.children[i])
			} else {
				tok = skipExcessElement(tok)
			}
		}
		return
	}

	init.expr = assign(rest, tok)
}

func initializer(rest **Token, tok *Token, ty *Type) *Initializer {
	init := NewInitializer(ty)
	initializer2(rest, tok, init)
	return init
}

func initDesgExpr(desg *InitDesg, tok *Token) *Node {
	if desg.vara != nil {
		return NewVarNode(desg.vara, tok)
	}

	lhs := initDesgExpr(desg.next, tok)
	rhs := NewNum(int64(desg.idx), tok)
	return NewUnary(ND_DEREF, newAdd(lhs, rhs, tok), tok)
}

func createLVarInit(init *Initializer, ty *Type, desg *InitDesg, tok *Token) *Node {
	if ty.kind == TY_ARRAY {
		node := NewNode(ND_NULL_EXPR, tok)
		for i := range ty.arrayLen {
			desg2 := InitDesg{next: desg, idx: i}
			rhs := createLVarInit(init.children[i], ty.base, &desg2, tok)
			node = NewBinary(ND_COMMA, node, rhs, tok)
		}
		return node
	}

	if init.expr == nil {
		return NewNode(ND_NULL_EXPR, tok)
	}

	lhs := initDesgExpr(desg, tok)
	return NewBinary(ND_ASSIGN, lhs, init.expr, tok)
}

// A variable definition with an initializer is a shorthand notation
// for a variable definition followed by assignments. This function
// generates assignment expressions for an initializer. For example,
// `int x[2][2] = {{6, 7}, {8, 9}}` is converted to the following
// expressions:
//
//	x[0][0] = 6;
//	x[0][1] = 7;
//	x[1][0] = 8;
//	x[1][1] = 9;
func lvarInitializer(rest **Token, tok *Token, vara *Obj) *Node {
	init := initializer(rest, tok, vara.ty)
	desg := InitDesg{nil, 0, vara}

	// If a partial initializer list is given, the standard requires
	// that unspecified elements are set to 0. Here, we simply
	// zero-initialize the entire memory region of a variable before
	// initializing it with user-supplied values.
	lhs := NewNode(ND_MEMZERO, tok)
	lhs.vara = vara

	rhs := createLVarInit(init, vara.ty, &desg, tok)
	return NewBinary(ND_COMMA, lhs, rhs, tok)
}

var typenames = []string{
	"void", "_Bool", "char", "short", "int", "long", "struct", "union",
	"typedef", "enum", "static",
}

// Returns true if a given token represents a type.
func isTypename(tok *Token) bool {
	for i := range typenames {
		if tok.equal(typenames[i]) {
			return true
		}
	}
	return findTypedef(tok) != nil
}

// stmt = "return" expr ";"
// | "if" "(" expr ")" stmt ("else" stmt)?
// | "switch" "(" expr ")" stmt
// | "case" const-expr ":" stmt
// | "default" ":" stmt
// | "for" "(" expr-stmt expr? ";" expr? ")" stmt
// | "while" "(" expr ")" stmt
// | "goto" ident ";"
// | "break" ";"
// | "continue" ";"
// | ident ":" stmt
// | "{" compound-stmt
// | expr-stmt
func stmt(rest **Token, tok *Token) *Node {
	if tok.equal("return") {
		node := NewNode(ND_RETURN, tok)
		exp := expr(&tok, tok.next)
		*rest = tok.skip(";")

		exp.addType()
		node.lhs = NewCast(exp, currentParseFn.ty.returnTy)
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

	if tok.equal("switch") {
		node := NewNode(ND_SWITCH, tok)
		tok = tok.next.skip("(")
		node.cond = expr(&tok, tok)
		tok = tok.skip(")")

		sw := currentSwitch
		currentSwitch = node

		brk := brkLabel
		node.brkLabel = newUniqueName()
		brkLabel = node.brkLabel

		node.then = stmt(rest, tok)

		currentSwitch = sw
		brkLabel = brk
		return node
	}

	if tok.equal("case") {
		if currentSwitch == nil {
			failTok(tok, "stray case")
		}

		node := NewNode(ND_CASE, tok)
		val := int32(constExpr(&tok, tok.next))
		tok = tok.skip(":")
		node.label = newUniqueName()
		node.lhs = stmt(rest, tok)
		node.val = int64(val)
		node.caseNext = currentSwitch.caseNext
		currentSwitch.caseNext = node
		return node
	}

	if tok.equal("default") {
		if currentSwitch == nil {
			failTok(tok, "stray default")
		}

		node := NewNode(ND_CASE, tok)
		tok = tok.next.skip(":")
		node.label = newUniqueName()
		node.lhs = stmt(rest, tok)
		currentSwitch.defaultCase = node
		return node
	}

	if tok.equal("for") {
		node := NewNode(ND_FOR, tok)
		tok = tok.next.skip("(")

		enterScope()

		brk := brkLabel
		cont := contLabel
		node.brkLabel = newUniqueName()
		brkLabel = node.brkLabel
		node.contLabel = newUniqueName()
		contLabel = node.contLabel

		if isTypename(tok) {
			basety := declspec(&tok, tok, nil)
			node.init = declaration(&tok, tok, basety)
		} else {
			node.init = exprStmt(&tok, tok)
		}

		if !tok.equal(";") {
			node.cond = expr(&tok, tok)
		}
		tok = tok.skip(";")

		if !tok.equal(")") {
			node.inc = expr(&tok, tok)
		}
		tok = tok.skip(")")

		node.then = stmt(rest, tok)

		leaveScope()
		brkLabel = brk
		contLabel = cont
		return node
	}

	if tok.equal("while") {
		node := NewNode(ND_FOR, tok)
		tok = tok.next.skip("(")
		node.cond = expr(&tok, tok)
		tok = tok.skip(")")

		brk := brkLabel
		cont := contLabel
		node.brkLabel = newUniqueName()
		brkLabel = node.brkLabel
		node.contLabel = newUniqueName()
		contLabel = node.contLabel

		node.then = stmt(rest, tok)

		brkLabel = brk
		contLabel = cont
		return node
	}

	if tok.equal("goto") {
		node := NewNode(ND_GOTO, tok)
		node.label = getIdent(tok.next)
		node.gotoNext = gotos
		gotos = node
		*rest = tok.next.next.skip(";")
		return node
	}

	if tok.equal("break") {
		if len(brkLabel) == 0 {
			failTok(tok, "stray break")
		}
		node := NewNode(ND_GOTO, tok)
		node.uniqueLabel = brkLabel
		*rest = tok.next.skip(";")
		return node
	}

	if tok.equal("continue") {
		if len(contLabel) == 0 {
			failTok(tok, "stray continue")
		}
		node := NewNode(ND_GOTO, tok)
		node.uniqueLabel = contLabel
		*rest = tok.next.skip(";")
		return node
	}

	if tok.kind == TK_IDENT && tok.next.equal(":") {
		node := NewNode(ND_LABEL, tok)
		node.label = tok.lexeme
		node.uniqueLabel = newUniqueName()
		node.lhs = stmt(rest, tok.next.next)
		node.gotoNext = labels
		labels = node
		return node
	}

	if tok.equal("{") {
		return compoundStmt(rest, tok.next)
	}

	return exprStmt(rest, tok)
}

// compound-stmt = (typedef | declaration | stmt)* "}"
func compoundStmt(rest **Token, tok *Token) *Node {
	node := NewNode(ND_BLOCK, tok)

	head := Node{}
	cur := &head

	enterScope()

	for !tok.equal("}") {
		if isTypename(tok) && !tok.next.equal(":") {
			attr := VarAttr{}
			basety := declspec(&tok, tok, &attr)

			if attr.isTypedef {
				tok = parseTypedef(tok, basety)
				continue
			}

			cur.next = declaration(&tok, tok, basety)
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

// Evaluate a given node as a constant expression.
func eval(node *Node) int64 {
	node.addType()

	switch node.kind {
	case ND_ADD:
		return eval(node.lhs) + eval(node.rhs)
	case ND_SUB:
		return eval(node.lhs) - eval(node.rhs)
	case ND_MUL:
		return eval(node.lhs) * eval(node.rhs)
	case ND_DIV:
		return eval(node.lhs) / eval(node.rhs)
	case ND_NEG:
		return -eval(node.lhs)
	case ND_MOD:
		return eval(node.lhs) % eval(node.rhs)
	case ND_BITAND:
		return eval(node.lhs) & eval(node.rhs)
	case ND_BITOR:
		return eval(node.lhs) | eval(node.rhs)
	case ND_BITXOR:
		return eval(node.lhs) ^ eval(node.rhs)
	case ND_SHL:
		return eval(node.lhs) << eval(node.rhs)
	case ND_SHR:
		return eval(node.lhs) >> eval(node.rhs)
	case ND_EQ:
		if eval(node.lhs) == eval(node.rhs) {
			return 1
		} else {
			return 0
		}
	case ND_NE:
		if eval(node.lhs) != eval(node.rhs) {
			return 1
		} else {
			return 0
		}
	case ND_LT:
		if eval(node.lhs) < eval(node.rhs) {
			return 1
		} else {
			return 0
		}
	case ND_LE:
		if eval(node.lhs) <= eval(node.rhs) {
			return 1
		} else {
			return 0
		}
	case ND_COND:
		if eval(node.cond) != 0 {
			return eval(node.then)
		} else {
			return eval(node.els)
		}
	case ND_COMMA:
		return eval(node.rhs)
	case ND_NOT:
		if eval(node.lhs) == 0 {
			return 1
		} else {
			return 0
		}
	case ND_BITNOT:
		return ^eval(node.lhs)
	case ND_LOGAND:
		if eval(node.lhs) != 0 && eval(node.rhs) != 0 {
			return 1
		} else {
			return 0
		}
	case ND_LOGOR:
		if eval(node.lhs) != 0 || eval(node.rhs) != 0 {
			return 1
		} else {
			return 0
		}
	case ND_CAST:
		if node.ty.isInteger() {
			switch node.ty.size {
			case 1:
				return int64(uint8(eval(node.lhs)))
			case 2:
				return int64(uint16(eval(node.lhs)))
			case 4:
				return int64(uint32(eval(node.lhs)))
			}
		}
		return eval(node.lhs)
	case ND_NUM:
		return node.val
	}

	failTok(node.tok, "not a compile-time constant")
	return -1
}

func constExpr(rest **Token, tok *Token) int64 {
	node := conditional(rest, tok)
	return eval(node)
}

// Convert `A op= B` to `tmp = &A, *tmp = *tmp op B`
// where tmp is a fresh pointer variable.
func toAssign(binary *Node) *Node {
	binary.lhs.addType()
	binary.rhs.addType()
	tok := binary.tok

	vara := NewLVar("", pointerTo(binary.lhs.ty))

	expr1 := NewBinary(ND_ASSIGN, NewVarNode(vara, tok),
		NewUnary(ND_ADDR, binary.lhs, tok), tok)

	expr2 := NewBinary(ND_ASSIGN, NewUnary(ND_DEREF, NewVarNode(vara, tok), tok),
		NewBinary(binary.kind,
			NewUnary(ND_DEREF, NewVarNode(vara, tok), tok),
			binary.rhs,
			tok),
		tok)

	return NewBinary(ND_COMMA, expr1, expr2, tok)
}

// assign    = conditional (assign-op assign)?
// assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
// | "<<=" | ">>="
func assign(rest **Token, tok *Token) *Node {
	node := conditional(&tok, tok)

	if tok.equal("=") {
		return NewBinary(ND_ASSIGN, node, assign(rest, tok.next), tok)
	}

	if tok.equal("+=") {
		return toAssign(newAdd(node, assign(rest, tok.next), tok))
	}

	if tok.equal("-=") {
		return toAssign(newSub(node, assign(rest, tok.next), tok))
	}

	if tok.equal("*=") {
		return toAssign(NewBinary(ND_MUL, node, assign(rest, tok.next), tok))
	}

	if tok.equal("/=") {
		return toAssign(NewBinary(ND_DIV, node, assign(rest, tok.next), tok))
	}

	if tok.equal("%=") {
		return toAssign(NewBinary(ND_MOD, node, assign(rest, tok.next), tok))
	}

	if tok.equal("&=") {
		return toAssign(NewBinary(ND_BITAND, node, assign(rest, tok.next), tok))
	}

	if tok.equal("|=") {
		return toAssign(NewBinary(ND_BITOR, node, assign(rest, tok.next), tok))
	}

	if tok.equal("^=") {
		return toAssign(NewBinary(ND_BITXOR, node, assign(rest, tok.next), tok))
	}

	if tok.equal("<<=") {
		return toAssign(NewBinary(ND_SHL, node, assign(rest, tok.next), tok))
	}

	if tok.equal(">>=") {
		return toAssign(NewBinary(ND_SHR, node, assign(rest, tok.next), tok))
	}

	*rest = tok
	return node
}

// conditional = logor ("?" expr ":" conditional)?
func conditional(rest **Token, tok *Token) *Node {
	cond := logor(&tok, tok)

	if !tok.equal("?") {
		*rest = tok
		return cond
	}

	node := NewNode(ND_COND, tok)
	node.cond = cond
	node.then = expr(&tok, tok.next)
	tok = tok.skip(":")
	node.els = conditional(rest, tok)
	return node
}

// logor = logand ("||" logand)*
func logor(rest **Token, tok *Token) *Node {
	node := logand(&tok, tok)
	for tok.equal("||") {
		start := tok
		node = NewBinary(ND_LOGOR, node, logand(&tok, tok.next), start)
	}
	*rest = tok
	return node
}

// logand = bitor ("&&" bitor)*
func logand(rest **Token, tok *Token) *Node {
	node := bitor(&tok, tok)
	for tok.equal("&&") {
		start := tok
		node = NewBinary(ND_LOGAND, node, bitor(&tok, tok.next), start)
	}
	*rest = tok
	return node
}

// bitor = bitxor ("|" bitxor)*
func bitor(rest **Token, tok *Token) *Node {
	node := bitxor(&tok, tok)
	for tok.equal("|") {
		start := tok
		node = NewBinary(ND_BITOR, node, bitxor(&tok, tok.next), start)
	}
	*rest = tok
	return node
}

// bitxor = bitand ("^" bitand)*
func bitxor(rest **Token, tok *Token) *Node {
	node := bitand(&tok, tok)
	for tok.equal("^") {
		start := tok
		node = NewBinary(ND_BITXOR, node, bitand(&tok, tok.next), start)
	}
	*rest = tok
	return node
}

// bitand = equality ("&" equality)*
func bitand(rest **Token, tok *Token) *Node {
	node := equality(&tok, tok)
	for tok.equal("&") {
		start := tok
		node = NewBinary(ND_BITAND, node, equality(&tok, tok.next), start)
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

// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
func relational(rest **Token, tok *Token) *Node {
	node := shift(&tok, tok)

	for {
		start := tok

		if tok.equal("<") {
			node = NewBinary(ND_LT, node, shift(&tok, tok.next), start)
			continue
		}

		if tok.equal("<=") {
			node = NewBinary(ND_LE, node, shift(&tok, tok.next), start)
			continue
		}

		if tok.equal(">") {
			node = NewBinary(ND_LT, shift(&tok, tok.next), node, start)
			continue
		}

		if tok.equal(">=") {
			node = NewBinary(ND_LE, shift(&tok, tok.next), node, start)
			continue
		}

		*rest = tok
		return node
	}
}

// shift = add ("<<" add | ">>" add)*
func shift(rest **Token, tok *Token) *Node {
	node := add(&tok, tok)

	for {
		start := tok

		if tok.equal("<<") {
			node = NewBinary(ND_SHL, node, add(&tok, tok.next), start)
			continue
		}

		if tok.equal(">>") {
			node = NewBinary(ND_SHR, node, add(&tok, tok.next), start)
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
	rhs = NewBinary(ND_MUL, rhs, NewLong(int64(lhs.ty.base.size), tok), tok)
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
		rhs = NewBinary(ND_MUL, rhs, NewLong(int64(lhs.ty.base.size), tok), tok)
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

// mul = cast ("*" cast | "/" cast | "%" cast)*
func mul(rest **Token, tok *Token) *Node {
	node := cast(&tok, tok)

	for {
		start := tok

		if tok.equal("*") {
			node = NewBinary(ND_MUL, node, cast(&tok, tok.next), start)
			continue
		}

		if tok.equal("/") {
			node = NewBinary(ND_DIV, node, cast(&tok, tok.next), start)
			continue
		}

		if tok.equal("%") {
			node = NewBinary(ND_MOD, node, cast(&tok, tok.next), start)
			continue
		}

		*rest = tok
		return node
	}
}

// cast = "(" type-name ")" cast | unary
func cast(rest **Token, tok *Token) *Node {
	if tok.equal("(") && isTypename(tok.next) {
		start := tok
		ty := typename(&tok, tok.next)
		tok = tok.skip(")")
		node := NewCast(cast(rest, tok), ty)
		node.tok = start
		return node
	}

	return unary(rest, tok)
}

// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
// | ("++" | "--") unary
// | postfix
func unary(rest **Token, tok *Token) *Node {
	if tok.equal("+") {
		return cast(rest, tok.next)
	}

	if tok.equal("-") {
		return NewUnary(ND_NEG, cast(rest, tok.next), tok)
	}

	if tok.equal("&") {
		return NewUnary(ND_ADDR, cast(rest, tok.next), tok)
	}

	if tok.equal("*") {
		return NewUnary(ND_DEREF, cast(rest, tok.next), tok)
	}

	if tok.equal("!") {
		return NewUnary(ND_NOT, cast(rest, tok.next), tok)
	}

	if tok.equal("~") {
		return NewUnary(ND_BITNOT, cast(rest, tok.next), tok)
	}

	// Read ++i as i+=1
	if tok.equal("++") {
		return toAssign(newAdd(unary(rest, tok.next), NewNum(1, tok), tok))
	}

	// Read --i as i-=1
	if tok.equal("--") {
		return toAssign(newSub(unary(rest, tok.next), NewNum(1, tok), tok))
	}

	return postfix(rest, tok)
}

// struct-members = (declspec declarator (","  declarator)* ";")*
func structMembers(rest **Token, tok *Token, ty *Type) {
	head := Member{}
	cur := &head

	for !tok.equal("}") {
		basety := declspec(&tok, tok, nil)
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
		*rest = tok

		ty := findTag(tag)
		if ty != nil {
			return ty
		}

		ty = structType()
		ty.size = -1
		pushTagScope(tag, ty)
		return ty
	}

	tok = tok.skip("{")

	// Construct a struct object.
	ty := structType()
	structMembers(rest, tok, ty)

	if tag != nil {
		// If this is a redefinition, overwrite a previous type.
		// Otherwise, register the struct type.
		for sc := scope.tags; sc != nil; sc = sc.next {
			if tag.equal(sc.name) {
				*sc.ty = *ty
				return sc.ty
			}
		}

		pushTagScope(tag, ty)
	}

	return ty
}

// struct-decl = struct-union-decl
func structDecl(rest **Token, tok *Token) *Type {
	ty := structUnionDecl(rest, tok)
	ty.kind = TY_STRUCT

	if ty.size < 0 {
		return ty
	}

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

	if ty.size < 0 {
		return ty
	}

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

// Convert A++ to `(typeof A)((A += 1) - 1)`
func newIncDec(node *Node, tok *Token, addend int) *Node {
	node.addType()
	return NewCast(newAdd(toAssign(newAdd(node, NewNum(int64(addend), tok), tok)),
		NewNum(int64(-addend), tok), tok),
		node.ty)
}

// postfix = primary ("[" expr "]" | "." ident | "->" ident | "++" | "--")*
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

		if tok.equal("++") {
			node = newIncDec(node, tok, 1)
			tok = tok.next
			continue
		}

		if tok.equal("--") {
			node = newIncDec(node, tok, -1)
			tok = tok.next
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

	sc := findVar(start)
	if sc == nil {
		failTok(start, "implicit declaration of a function")
	}
	if sc.vara == nil || sc.vara.ty.kind != TY_FUNC {
		failTok(start, "not a function")
	}

	ty := sc.vara.ty
	paramty := ty.params

	head := Node{}
	cur := &head

	for !tok.equal(")") {
		if cur != &head {
			tok = tok.skip(",")
		}

		arg := assign(&tok, tok)
		arg.addType()

		if paramty != nil {
			if paramty.kind == TY_STRUCT || paramty.kind == TY_UNION {
				failTok(arg.tok, "passing struct or union is not supported yet")
			}
			arg = NewCast(arg, paramty)
			paramty = paramty.next
		}

		cur.next = arg
		cur = cur.next
	}

	*rest = tok.skip(")")

	node := NewNode(ND_FUNCALL, start)
	node.funcname = start.lexeme
	node.functy = ty
	node.ty = ty.returnTy
	node.args = head.next
	return node
}

// primary = "(" "{" stmt+ "}" ")"
// | "(" expr ")"
// | "sizeof" "(" type-name ")"
// | "sizeof" unary
// | ident func-args?
// | str
// | num
func primary(rest **Token, tok *Token) *Node {
	start := tok

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

	if tok.equal("sizeof") && tok.next.equal("(") && isTypename(tok.next.next) {
		ty := typename(&tok, tok.next.next)
		*rest = tok.skip(")")
		return NewNum(int64(ty.size), start)
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

		// Variable or enum constant
		sc := findVar(tok)
		if sc == nil || (sc.vara == nil && sc.enumty == nil) {
			failTok(tok, "undefined variable")
		}

		var node *Node
		if sc.vara != nil {
			node = NewVarNode(sc.vara, tok)
		} else {
			node = NewNum(int64(sc.enumval), tok)
		}

		*rest = tok.next
		return node
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

func parseTypedef(tok *Token, basety *Type) *Token {
	first := true

	for !consume(&tok, tok, ";") {
		if !first {
			tok = tok.skip(",")
		}
		first = false

		ty := declarator(&tok, tok, basety)
		pushScope(getIdent(ty.name)).typedef = ty
	}
	return tok
}

func createParamLvars(param *Type) {
	if param != nil {
		createParamLvars(param.next)
		NewLVar(getIdent(param.name), param)
	}
}

// This function matches gotos with labels.

// We cannot resolve gotos as we parse a function because gotos
// can refer a label that appears later in the function.
// So, we need to do this after we parse the entire function.
func resolveGotoLabels() {
	for x := gotos; x != nil; x = x.gotoNext {
		for y := labels; y != nil; y = y.gotoNext {
			if x.label == y.label {
				x.uniqueLabel = y.uniqueLabel
				break
			}
		}

		if len(x.uniqueLabel) == 0 {
			failTok(x.tok.next, "use of undeclared label")
		}
	}
	labels = nil
	gotos = nil
}

func function(tok *Token, basety *Type, attr *VarAttr) *Token {
	ty := declarator(&tok, tok, basety)

	fn := NewGVar(getIdent(ty.name), ty)
	fn.isFunction = true
	fn.isDefinition = !consume(&tok, tok, ";")
	fn.isStatic = attr.isStatic

	if !fn.isDefinition {
		return tok
	}

	currentParseFn = fn
	locals = nil
	enterScope()
	createParamLvars(ty.params)
	fn.params = locals

	tok = tok.skip("{")
	fn.body = compoundStmt(&tok, tok)
	fn.locals = locals
	leaveScope()
	resolveGotoLabels()
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

// program = (typedef | function-definition | global-variable)*
func parse(tok *Token) *Obj {
	globals = nil

	for tok.kind != TK_EOF {
		attr := VarAttr{}
		basety := declspec(&tok, tok, &attr)

		// Typedef
		if attr.isTypedef {
			tok = parseTypedef(tok, basety)
			continue
		}

		// Function
		if isFunction(tok) {
			tok = function(tok, basety, &attr)
			continue
		}

		// Global variable
		tok = globalVariable(tok, basety)
	}

	return globals
}
