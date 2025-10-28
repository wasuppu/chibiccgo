package main

import (
	"encoding/binary"
	"fmt"
	"math"
)

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
	next       *Initializer
	ty         *Type
	tok        *Token
	isFlexible bool

	// If it's not an aggregate type and has an initializer,
	// `expr` has an initialization expression.
	expr *Node

	// If it's an initializer for an aggregate type (e.g. array or struct),
	// `children` has initializers for its children.
	children []*Initializer
}

// For local variable initializer.
type InitDesg struct {
	next   *InitDesg
	idx    int
	member *Member
	vara   *Obj
}

// Struct member
type Member struct {
	next   *Member
	ty     *Type
	name   *Token
	idx    int
	align  int
	offset int
}

// Variable or function
type Obj struct {
	next    *Obj
	name    string // Variable name
	ty      *Type  // Type
	tok     *Token // representative token
	isLocal bool   // local or global/function
	align   int    // alignment

	// Local variable
	offset int

	// struct type for riscv
	isHalfByStack bool

	// Global variable or function
	isFunction   bool
	isDefinition bool
	isStatic     bool

	// Global variable
	initData string
	rel      *Relocation

	// Function
	params    *Obj
	body      *Node
	locals    *Obj
	vaArea    *Obj
	stackSize int
}

// Global variable can be initialized either by a constant expression
// or a pointer to another global variable. This struct represents the
// latter.
type Relocation struct {
	next   *Relocation
	offset int
	label  string
	addend int64
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
	ND_DO                        // "do"
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
	functy      *Type
	args        *Node
	passByStack bool
	retBuffer   *Obj

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
	val  int64
	fval float64
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
	isExtern  bool
	align     int
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

func NewULong(val int64, tok *Token) *Node {
	node := NewNode(ND_NUM, tok)
	node.val = val
	node.ty = tyULong
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

func NewInitializer(ty *Type, isFlexible bool) *Initializer {
	init := &Initializer{}
	init.ty = ty

	if ty.kind == TY_ARRAY {
		if isFlexible && ty.size < 0 {
			init.isFlexible = true
			return init
		}

		init.children = make([]*Initializer, ty.arrayLen)
		for i := range ty.arrayLen {
			init.children[i] = NewInitializer(ty.base, false)
		}
		return init
	}

	if ty.kind == TY_STRUCT || ty.kind == TY_UNION {
		// Count the number of struct members.
		len := 0
		for mem := ty.members; mem != nil; mem = mem.next {
			len++
		}

		init.children = make([]*Initializer, len)
		for mem := ty.members; mem != nil; mem = mem.next {
			if isFlexible && ty.isFlexible && mem.next == nil {
				child := &Initializer{ty: mem.ty, isFlexible: true}
				init.children[mem.idx] = child
			} else {
				init.children[mem.idx] = NewInitializer(mem.ty, false)
			}
		}
		return init
	}

	return init
}

func NewVar(name string, ty *Type) *Obj {
	vara := &Obj{
		name:  name,
		ty:    ty,
		align: ty.align,
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
	vara.isStatic = true
	vara.isDefinition = true
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
	VOID     = 1 << 0
	BOOL     = 1 << 2
	CHAR     = 1 << 4
	SHORT    = 1 << 6
	INT      = 1 << 8
	LONG     = 1 << 10
	FLOAT    = 1 << 12
	DOUBLE   = 1 << 14
	OTHER    = 1 << 16
	SIGNED   = 1 << 17
	UNSIGNED = 1 << 18
)

// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
// | "typedef" | "static" | "extern"
// | "signed" | "unsigned"
// | struct-decl | union-decl | typedef-name
// | enum-specifier
// | "const" | "volatile" | "auto" | "register" | "restrict"
// | "__restrict" | "__restrict__" | "_Noreturn")+
func declspec(rest **Token, tok *Token, attr *VarAttr) *Type {
	ty := tyInt
	counter := 0

	for isTypename(tok) {
		// Handle storage class specifiers.
		if tok.equal("typedef") || tok.equal("static") || tok.equal("extern") {
			if attr == nil {
				failTok(tok, "storage class specifier is not allowed in this context")
			}

			if tok.equal("typedef") {
				attr.isTypedef = true
			} else if tok.equal("static") {
				attr.isStatic = true
			} else {
				attr.isExtern = true
			}

			if attr.isTypedef && (attr.isStatic && attr.isExtern) {
				failTok(tok, "typedef and static may not be used together with static or extern")
			}
			tok = tok.next
			continue
		}

		// These keywords are recognized but ignored.
		if consume(&tok, tok, "const") || consume(&tok, tok, "volatile") ||
			consume(&tok, tok, "auto") || consume(&tok, tok, "register") ||
			consume(&tok, tok, "restrict") || consume(&tok, tok, "__restrict") ||
			consume(&tok, tok, "__restrict__") || consume(&tok, tok, "_Noreturn") {
			continue
		}

		if tok.equal("_Alignas") {
			if attr == nil {
				failTok(tok, "_Alignas is not allowed in this context")
			}
			tok = tok.next.skip("(")

			if isTypename(tok) {
				attr.align = typename(&tok, tok).align
			} else {
				attr.align = int(constExpr(&tok, tok))
			}
			tok = tok.skip(")")
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
		} else if tok.equal("float") {
			counter += FLOAT
		} else if tok.equal("double") {
			counter += DOUBLE
		} else if tok.equal("signed") {
			counter |= SIGNED
		} else if tok.equal("unsigned") {
			counter |= UNSIGNED
		} else {
			unreachable()
		}

		switch counter {
		case VOID:
			ty = tyVoid
		case BOOL:
			ty = tyBool
		case CHAR, SIGNED + CHAR:
			ty = tyChar
		case UNSIGNED + CHAR:
			ty = tyUChar
		case SHORT, SHORT + INT,
			SIGNED + SHORT,
			SIGNED + SHORT + INT:
			ty = tyShort
		case UNSIGNED + SHORT,
			UNSIGNED + SHORT + INT:
			ty = tyUShort
		case INT, SIGNED, SIGNED + INT:
			ty = tyInt
		case UNSIGNED, UNSIGNED + INT:
			ty = tyUInt
		case LONG, LONG + INT,
			LONG + LONG,
			LONG + LONG + INT,
			SIGNED + LONG,
			SIGNED + LONG + INT,
			SIGNED + LONG + LONG,
			SIGNED + LONG + LONG + INT:
			ty = tyLong
		case UNSIGNED + LONG,
			UNSIGNED + LONG + INT,
			UNSIGNED + LONG + LONG,
			UNSIGNED + LONG + LONG + INT:
			ty = tyULong
		case FLOAT:
			ty = tyFloat
		case DOUBLE, LONG + DOUBLE:
			ty = tyDouble
		default:
			failTok(tok, "invalid type")
		}

		tok = tok.next
	}

	*rest = tok
	return ty
}

// func-params = ("void" | param ("," param)* ("," "...")?)? ")"
// param       = declspec declarator
func funcParams(rest **Token, tok *Token, ty *Type) *Type {
	if tok.equal("void") && tok.next.equal(")") {
		*rest = tok.next.next
		return funcType(ty)
	}

	head := Type{}
	cur := &head
	isVariadic := false

	for !tok.equal(")") {
		if cur != &head {
			tok = tok.skip(",")
		}

		if tok.equal("...") {
			isVariadic = true
			tok = tok.next
			tok.skip(")")
			break
		}

		ty2 := declspec(&tok, tok, nil)
		ty2 = declarator(&tok, tok, ty2)

		name := ty2.name

		switch ty2.kind {
		case TY_ARRAY:
			// "array of T" is converted to "pointer to T" only in the parameter
			// context. For example, *argv[] is converted to **argv by this.
			ty2 = pointerTo(ty2.base)
			ty2.name = name
		case TY_FUNC:
			// Likewise, a function is converted to a pointer to a function
			// only in the parameter context.
			ty2 = pointerTo(ty2)
			ty2.name = name
		}

		cur.next = copyType(ty2)
		cur = cur.next
	}

	if cur == &head {
		isVariadic = true
	}

	ty = funcType(ty)
	ty.params = head.next
	ty.isVariadic = isVariadic
	*rest = tok.next
	return ty
}

// array-dimensions = ("static" | "restrict")* const-expr? "]" type-suffix
func arrayDimensions(rest **Token, tok *Token, ty *Type) *Type {
	for tok.equal("static") || tok.equal("restrict") {
		tok = tok.next
	}

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

// pointers = ("*" ("const" | "volatile" | "restrict")*)*
func pointers(rest **Token, tok *Token, ty *Type) *Type {
	for consume(&tok, tok, "*") {
		ty = pointerTo(ty)
		for tok.equal("const") || tok.equal("volatile") || tok.equal("restrict") ||
			tok.equal("__restrict") || tok.equal("__restrict__") {
			tok = tok.next
		}
	}
	*rest = tok
	return ty
}

// declarator = pointers ("(" ident ")" | "(" declarator ")" | ident) type-suffix
func declarator(rest **Token, tok *Token, ty *Type) *Type {
	ty = pointers(&tok, tok, ty)

	if tok.equal("(") {
		start := tok
		dummy := Type{}
		declarator(&tok, start.next, &dummy)
		tok = tok.skip(")")
		ty = typeSuffix(rest, tok, ty)
		return declarator(&tok, start.next, ty)
	}

	var name *Token
	namePos := tok

	if tok.kind == TK_IDENT {
		name = tok
		tok = tok.next
	}

	ty = typeSuffix(rest, tok, ty)
	ty.name = name
	ty.namePos = namePos
	return ty
}

// abstract-declarator = pointers ("(" abstract-declarator ")")? type-suffix
func abstractDeclarator(rest **Token, tok *Token, ty *Type) *Type {
	ty = pointers(&tok, tok, ty)

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

func isEnd(tok *Token) bool {
	return tok.equal("}") || tok.equal(",") && tok.next.equal("}")
}

func consumeEnd(rest **Token, tok *Token) bool {
	if tok.equal("}") {
		*rest = tok.next
		return true
	}

	if tok.equal(",") && tok.next.equal("}") {
		*rest = tok.next.next
		return true
	}

	return false
}

// enum-specifier = ident? "{" enum-list? "}"
// | ident ("{" enum-list? "}")?
// enum-list      = ident ("=" num)? ("," ident ("=" num)?)* ","?
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
	for !consumeEnd(rest, tok) {
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

	if tag != nil {
		pushTagScope(tag, ty)
	}
	return ty
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
func declaration(rest **Token, tok *Token, basety *Type, attr *VarAttr) *Node {
	head := Node{}
	cur := &head
	i := 0

	for !tok.equal(";") {
		if i > 0 {
			tok = tok.skip(",")
		}
		i++

		ty := declarator(&tok, tok, basety)
		if ty.kind == TY_VOID {
			failTok(tok, "variable declared void")
		}
		if ty.name == nil {
			failTok(ty.namePos, "variable name omitted")
		}

		if attr != nil && attr.isStatic {
			// static local variable
			vara := newAnonGVar(ty)
			pushScope(getIdent(ty.name)).vara = vara
			if tok.equal("=") {
				gvarInitializer(&tok, tok.next, vara)
			}
			continue
		}

		vara := NewLVar(getIdent(ty.name), ty)
		if attr != nil && attr.align != 0 {
			vara.align = attr.align
		}

		if tok.equal("=") {
			expr := lvarInitializer(&tok, tok.next, vara)
			cur.next = NewUnary(ND_EXPR_STMT, expr, tok)
			cur = cur.next
		}

		if vara.ty.size < 0 {
			failTok(ty.name, "variable has incomplete type")
		}
		if vara.ty.kind == TY_VOID {
			failTok(ty.name, "variable declared void")
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

// string-initializer = string-literal
func stringInitializer(rest **Token, tok *Token, init *Initializer) {
	if init.isFlexible {
		*init = *NewInitializer(arrayOf(init.ty.base, tok.ty.arrayLen), false)
	}

	len := min(init.ty.arrayLen, tok.ty.arrayLen)
	for i := range len {
		init.children[i].expr = NewNum(int64(tok.str[i]), tok)
	}
	*rest = tok.next
}

func countArrayInitElements(tok *Token, ty *Type) int {
	dummy := NewInitializer(ty.base, false)
	i := 0

	for ; !consumeEnd(&tok, tok); i++ {
		if i > 0 {
			tok = tok.skip(",")
		}
		initializer2(&tok, tok, dummy)
	}
	return i
}

// array-initializer1 = "{" initializer ("," initializer)* ","? "}"
func arrayInitializer1(rest **Token, tok *Token, init *Initializer) {
	tok = tok.skip("{")

	if init.isFlexible {
		len := countArrayInitElements(tok, init.ty)
		*init = *NewInitializer(arrayOf(init.ty.base, len), false)
	}

	for i := 0; !consumeEnd(rest, tok); i++ {
		if i > 0 {
			tok = tok.skip(",")
		}

		if i < init.ty.arrayLen {
			initializer2(&tok, tok, init.children[i])
		} else {
			tok = skipExcessElement(tok)
		}
	}
}

// array-initializer2 = initializer ("," initializer)*
func arrayInitializer2(rest **Token, tok *Token, init *Initializer) {
	if init.isFlexible {
		len := countArrayInitElements(tok, init.ty)
		*init = *NewInitializer(arrayOf(init.ty.base, len), false)
	}

	for i := 0; i < init.ty.arrayLen && !isEnd(tok); i++ {
		if i > 0 {
			tok = tok.skip(",")
		}
		initializer2(&tok, tok, init.children[i])
	}
	*rest = tok
}

// struct-initializer1 = "{" initializer ("," initializer)* ","? "}"
func structInitializer1(rest **Token, tok *Token, init *Initializer) {
	tok = tok.skip("{")

	mem := init.ty.members

	for !consumeEnd(rest, tok) {
		if mem != init.ty.members {
			tok = tok.skip(",")
		}

		if mem != nil {
			initializer2(&tok, tok, init.children[mem.idx])
			mem = mem.next
		} else {
			tok = skipExcessElement(tok)
		}
	}
}

// struct-initializer2 = initializer ("," initializer)*
func structInitializer2(rest **Token, tok *Token, init *Initializer) {
	first := true

	for mem := init.ty.members; mem != nil && !isEnd(tok); mem = mem.next {
		if !first {
			tok = tok.skip(",")
		}
		first = false
		initializer2(&tok, tok, init.children[mem.idx])
	}
	*rest = tok
}

func unionInitializer(rest **Token, tok *Token, init *Initializer) {
	// Unlike structs, union initializers take only one initializer,
	// and that initializes the first union member.
	if tok.equal("{") {
		initializer2(&tok, tok.next, init.children[0])
		consume(&tok, tok, ",")
		*rest = tok.skip("}")
	} else {
		initializer2(rest, tok, init.children[0])
	}
}

// initializer = string-initializer | array-initializer
// | struct-initializer | union-initializer
// | assign
func initializer2(rest **Token, tok *Token, init *Initializer) {
	if init.ty.kind == TY_ARRAY && tok.kind == TK_STR {
		stringInitializer(rest, tok, init)
		return
	}

	if init.ty.kind == TY_ARRAY {
		if tok.equal("{") {
			arrayInitializer1(rest, tok, init)
		} else {
			arrayInitializer2(rest, tok, init)
		}
		return
	}

	if init.ty.kind == TY_STRUCT {
		if tok.equal("{") {
			structInitializer1(rest, tok, init)
			return
		}

		// A struct can be initialized with another struct. E.g.
		// `struct T x = y;` where y is a variable of type `struct T`.
		// Handle that case first.
		expr := assign(rest, tok)
		expr.addType()
		if expr.ty.kind == TY_STRUCT {
			init.expr = expr
			return
		}

		structInitializer2(rest, tok, init)
		return
	}

	if init.ty.kind == TY_UNION {
		unionInitializer(rest, tok, init)
		return
	}

	if tok.equal("{") {
		// An initializer for a scalar variable can be surrounded by
		// braces. E.g. `int x = {3};`. Handle that case.
		initializer2(&tok, tok.next, init)
		*rest = tok.skip("}")
		return
	}

	init.expr = assign(rest, tok)
}

func copyStructType(ty *Type) *Type {
	ty = copyType(ty)

	head := Member{}
	cur := &head
	for mem := ty.members; mem != nil; mem = mem.next {
		m := &Member{}
		*m = *mem
		cur.next = m
		cur = cur.next
	}

	ty.members = head.next
	return ty
}

func initializer(rest **Token, tok *Token, ty *Type, newty **Type) *Initializer {
	init := NewInitializer(ty, true)
	initializer2(rest, tok, init)

	if (ty.kind == TY_STRUCT || ty.kind == TY_UNION) && ty.isFlexible {
		ty = copyStructType(ty)

		mem := ty.members
		for mem.next != nil {
			mem = mem.next
		}
		mem.ty = init.children[mem.idx].ty
		ty.size += mem.ty.size

		*newty = ty
		return init
	}

	*newty = init.ty
	return init
}

func initDesgExpr(desg *InitDesg, tok *Token) *Node {
	if desg.vara != nil {
		return NewVarNode(desg.vara, tok)
	}

	if desg.member != nil {
		node := NewUnary(ND_MEMBER, initDesgExpr(desg.next, tok), tok)
		node.member = desg.member
		return node
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

	if ty.kind == TY_STRUCT && init.expr == nil {
		node := NewNode(ND_NULL_EXPR, tok)

		for mem := ty.members; mem != nil; mem = mem.next {
			desg2 := InitDesg{next: desg, idx: 0, member: mem}
			rhs := createLVarInit(init.children[mem.idx], mem.ty, &desg2, tok)
			node = NewBinary(ND_COMMA, node, rhs, tok)
		}
		return node
	}

	if ty.kind == TY_UNION {
		desg2 := InitDesg{next: desg, idx: 0, member: ty.members}
		return createLVarInit(init.children[0], ty.members.ty, &desg2, tok)
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
	init := initializer(rest, tok, vara.ty, &vara.ty)
	desg := InitDesg{nil, 0, nil, vara}

	// If a partial initializer list is given, the standard requires
	// that unspecified elements are set to 0. Here, we simply
	// zero-initialize the entire memory region of a variable before
	// initializing it with user-supplied values.
	lhs := NewNode(ND_MEMZERO, tok)
	lhs.vara = vara

	rhs := createLVarInit(init, vara.ty, &desg, tok)
	return NewBinary(ND_COMMA, lhs, rhs, tok)
}

func writeBuf(buf []byte, val uint64, sz int) {
	switch sz {
	case 1:
		buf[0] = byte(val)
	case 2:
		binary.LittleEndian.PutUint16(buf, uint16(val))
	case 4:
		binary.LittleEndian.PutUint32(buf, uint32(val))
	case 8:
		binary.LittleEndian.PutUint64(buf, val)
	default:
		unreachable()
	}
}

func writeGVarData(cur *Relocation, init *Initializer, ty *Type, buf []byte, offset int) *Relocation {
	if ty.kind == TY_ARRAY {
		sz := ty.base.size
		for i := range ty.arrayLen {
			cur = writeGVarData(cur, init.children[i], ty.base, buf, offset+sz*i)
		}
		return cur
	}

	if ty.kind == TY_STRUCT {
		for mem := ty.members; mem != nil; mem = mem.next {
			cur = writeGVarData(cur, init.children[mem.idx], mem.ty, buf, offset+mem.offset)
		}
		return cur
	}

	if ty.kind == TY_UNION {
		return writeGVarData(cur, init.children[0], ty.members.ty, buf, offset)
	}

	if init.expr == nil {
		return cur
	}

	if ty.kind == TY_FLOAT {
		binary.LittleEndian.PutUint32(buf[offset:], math.Float32bits(float32(evalDouble(init.expr))))
		return cur
	}

	if ty.kind == TY_DOUBLE {
		binary.LittleEndian.PutUint64(buf[offset:], math.Float64bits(evalDouble(init.expr)))
		return cur
	}

	var label string
	val := uint64(eval2(init.expr, &label))

	if len(label) == 0 {
		writeBuf(buf[offset:], val, ty.size)
		return cur
	}

	rel := &Relocation{offset: offset, label: label, addend: int64(val)}
	cur.next = rel
	return cur.next
}

// Initializers for global variables are evaluated at compile-time and
// embedded to .data section. This function serializes Initializer
// objects to a flat byte array. It is a compile error if an
// initializer list contains a non-constant expression.
func gvarInitializer(rest **Token, tok *Token, vara *Obj) {
	init := initializer(rest, tok, vara.ty, &vara.ty)

	head := Relocation{}
	buf := make([]byte, vara.ty.size)
	writeGVarData(&head, init, vara.ty, buf, 0)
	vara.initData = string(buf)
	vara.rel = head.next
}

var typenames = []string{
	"void", "_Bool", "char", "short", "int", "long", "struct", "union",
	"typedef", "enum", "static", "extern", "_Alignas", "signed", "unsigned",
	"const", "volatile", "auto", "register", "restrict", "__restrict",
	"__restrict__", "_Noreturn", "float", "double",
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

// stmt = "return" expr? ";"
// | "if" "(" expr ")" stmt ("else" stmt)?
// | "switch" "(" expr ")" stmt
// | "case" const-expr ":" stmt
// | "default" ":" stmt
// | "for" "(" expr-stmt expr? ";" expr? ")" stmt
// | "while" "(" expr ")" stmt
// | "do" stmt "while" "(" expr ")" ";"
// | "goto" ident ";"
// | "break" ";"
// | "continue" ";"
// | ident ":" stmt
// | "{" compound-stmt
// | expr-stmt
func stmt(rest **Token, tok *Token) *Node {
	if tok.equal("return") {
		node := NewNode(ND_RETURN, tok)
		if consume(rest, tok.next, ";") {
			return node
		}

		exp := expr(&tok, tok.next)
		*rest = tok.skip(";")

		exp.addType()
		ty := currentParseFn.ty.returnTy
		if ty.kind != TY_STRUCT && ty.kind != TY_UNION {
			exp = NewCast(exp, currentParseFn.ty.returnTy)
		}
		node.lhs = exp
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
			node.init = declaration(&tok, tok, basety, nil)
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

	if tok.equal("do") {
		node := NewNode(ND_DO, tok)

		brk := brkLabel
		cont := contLabel
		node.brkLabel = newUniqueName()
		brkLabel = node.brkLabel
		node.contLabel = newUniqueName()
		contLabel = node.contLabel

		node.then = stmt(&tok, tok.next)

		brkLabel = brk
		contLabel = cont

		tok = tok.skip("while")
		tok = tok.skip("(")
		node.cond = expr(&tok, tok)
		tok = tok.skip(")")
		*rest = tok.skip(";")
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

			if isFunction(tok) {
				tok = function(tok, basety, &attr)
				continue
			}

			if attr.isExtern {
				tok = globalVariable(tok, basety, &attr)
				continue
			}

			cur.next = declaration(&tok, tok, basety, &attr)
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
	var label string
	return eval2(node, &label)
}

// Evaluate a given node as a constant expression.
//
// A constant expression is either just a number or ptr+n where ptr
// is a pointer to a global variable and n is a postiive/negative
// number. The latter form is accepted only as an initialization
// expression for a global variable.
func eval2(node *Node, label *string) int64 {
	node.addType()

	if node.ty.isFlonum() {
		return int64(evalDouble(node))
	}

	switch node.kind {
	case ND_ADD:
		return eval2(node.lhs, label) + eval(node.rhs)
	case ND_SUB:
		return eval2(node.lhs, label) - eval(node.rhs)
	case ND_MUL:
		return eval(node.lhs) * eval(node.rhs)
	case ND_DIV:
		if node.ty.isUnsigned {
			return int64(uint64(eval(node.lhs)) / uint64(eval(node.rhs)))
		}
		return eval(node.lhs) / eval(node.rhs)
	case ND_NEG:
		return -eval(node.lhs)
	case ND_MOD:
		if node.ty.isUnsigned {
			return int64(uint64(eval(node.lhs)) % uint64(eval(node.rhs)))
		}
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
		if node.ty.isUnsigned && node.ty.size == 8 {
			return int64(uint64(eval(node.lhs)) >> uint64(eval(node.rhs)))
		}
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
		if node.ty.isUnsigned {
			if uint64(eval(node.lhs)) < uint64(eval(node.rhs)) {
				return 1
			} else {
				return 0
			}
		}
		if eval(node.lhs) < eval(node.rhs) {
			return 1
		} else {
			return 0
		}
	case ND_LE:
		if node.ty.isUnsigned {
			if uint64(eval(node.lhs)) <= uint64(eval(node.rhs)) {
				return 1
			} else {
				return 0
			}
		}
		if eval(node.lhs) <= eval(node.rhs) {
			return 1
		} else {
			return 0
		}
	case ND_COND:
		if eval(node.cond) != 0 {
			return eval2(node.then, label)
		} else {
			return eval2(node.els, label)
		}
	case ND_COMMA:
		return eval2(node.rhs, label)
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
		val := eval2(node.lhs, label)
		if node.ty.isInteger() {
			switch node.ty.size {
			case 1:
				if node.ty.isUnsigned {
					return int64(uint8(val))
				} else {
					return int64(int8(val))
				}
			case 2:
				if node.ty.isUnsigned {
					return int64(uint16(val))
				} else {
					return int64(int16(val))
				}
			case 4:
				if node.ty.isUnsigned {
					return int64(uint32(val))
				} else {
					return int64(int32(val))
				}
			}
		}
		return val
	case ND_ADDR:
		return evalRVal(node.lhs, label)
	case ND_MEMBER:
		if len(*label) != 0 {
			failTok(node.tok, "not a compile-time constant ndmember")
		}
		if node.ty.kind != TY_ARRAY {
			failTok(node.tok, "invalid initializer")
		}
		return evalRVal(node.lhs, label) + int64(node.member.offset)
	case ND_VAR:
		if len(*label) != 0 {
			failTok(node.tok, "not a compile-time constant ndvar")
		}
		if node.vara.ty.kind != TY_ARRAY && node.vara.ty.kind != TY_FUNC {
			failTok(node.tok, "invalid initializer")
		}
		*label = node.vara.name
		return 0
	case ND_NUM:
		return node.val
	}

	failTok(node.tok, "not a compile-time constant")
	return -1
}

func evalRVal(node *Node, label *string) int64 {
	switch node.kind {
	case ND_VAR:
		if node.vara.isLocal {
			failTok(node.tok, "not a compile-time constant evalrel")
		}
		*label = node.vara.name
		return 0
	case ND_DEREF:
		return eval2(node.lhs, label)
	case ND_MEMBER:
		return evalRVal(node.lhs, label) + int64(node.member.offset)
	}

	failTok(node.tok, "invalid initializer")
	return -1
}

func constExpr(rest **Token, tok *Token) int64 {
	node := conditional(rest, tok)
	return eval(node)
}

func evalDouble(node *Node) float64 {
	node.addType()

	if node.ty.isInteger() {
		if node.ty.isUnsigned {
			return float64(uint64(eval(node)))
		}
		return float64(eval(node))
	}

	switch node.kind {
	case ND_ADD:
		return evalDouble(node.lhs) + evalDouble(node.rhs)
	case ND_SUB:
		return evalDouble(node.lhs) - evalDouble(node.rhs)
	case ND_MUL:
		return evalDouble(node.lhs) * evalDouble(node.rhs)
	case ND_DIV:
		return evalDouble(node.lhs) / evalDouble(node.rhs)
	case ND_NEG:
		return -evalDouble(node.lhs)
	case ND_COND:
		if evalDouble(node.cond) != 0 {
			return evalDouble(node.then)
		} else {
			return evalDouble(node.els)
		}
	case ND_COMMA:
		return evalDouble(node.rhs)
	case ND_CAST:
		if node.lhs.ty.isFlonum() {
			return evalDouble(node.lhs)
		}
		return float64(eval(node.lhs))
	case ND_NUM:
		return node.fval
	}

	failTok(node.tok, "not a compile-time constant")
	return -1
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
	if lhs.ty.isNumeric() && rhs.ty.isNumeric() {
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
	if lhs.ty.isNumeric() && rhs.ty.isNumeric() {
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
		node.ty = tyLong
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

		// compound literal
		if tok.equal("{") {
			return unary(rest, start)
		}

		// type cast
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
		// [https://www.sigbus.info/n1570#6.5.3.2p4] This is an oddity
		// in the C spec, but dereferencing a function shouldn't do
		// anything. If foo is a function, `*foo`, `**foo` or `*****foo`
		// are all equivalent to just `foo`.
		node := cast(rest, tok.next)
		node.addType()
		if node.ty.kind == TY_FUNC {
			return node
		}
		return NewUnary(ND_DEREF, node, tok)
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
	idx := 0

	for !tok.equal("}") {
		attr := VarAttr{}
		basety := declspec(&tok, tok, &attr)
		first := true

		for !consume(&tok, tok, ";") {
			if !first {
				tok = tok.skip(",")
			}
			first = false

			mem := &Member{}
			mem.ty = declarator(&tok, tok, basety)
			mem.name = mem.ty.name
			mem.idx = idx
			idx++
			if attr.align != 0 {
				mem.align = attr.align
			} else {
				mem.align = mem.ty.align
			}
			cur.next = mem
			cur = cur.next
		}
	}

	// If the last element is an array of incomplete type, it's
	// called a "flexible array member". It should behave as if
	// if were a zero-sized array.
	if cur != &head && cur.ty.kind == TY_ARRAY && cur.ty.arrayLen < 0 {
		cur.ty = arrayOf(cur.ty.base, 0)
		ty.isFlexible = true
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
		offset = alignTo(offset, mem.align)
		mem.offset = offset
		offset += mem.ty.size

		if ty.align < mem.align {
			ty.align = mem.align
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
		if ty.align < mem.align {
			ty.align = mem.align
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

// postfix = "(" type-name ")" "{" initializer-list "}"
// | ident "(" func-args ")" postfix-tail*
// | primary postfix-tail*
//
// postfix-tail = "[" expr "]"
// | "(" func-args ")"
// | "." ident
// | "->" ident
// | "++"
// | "--"
func postfix(rest **Token, tok *Token) *Node {
	if tok.equal("(") && isTypename(tok.next) {
		// Compound literal
		start := tok
		ty := typename(&tok, tok.next)
		tok = tok.skip(")")

		if scope.next == nil {
			vara := newAnonGVar(ty)
			gvarInitializer(rest, tok, vara)
			return NewVarNode(vara, start)
		}

		vara := NewLVar("", ty)
		lhs := lvarInitializer(rest, tok, vara)
		rhs := NewVarNode(vara, tok)
		return NewBinary(ND_COMMA, lhs, rhs, start)
	}

	node := primary(&tok, tok)

	for {
		if tok.equal("(") {
			node = funcall(&tok, tok.next, node)
			continue
		}

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

// funcall = (assign ("," assign)*)? ")"
func funcall(rest **Token, tok *Token, fn *Node) *Node {
	fn.addType()

	if fn.ty.kind != TY_FUNC && (fn.ty.kind != TY_PTR || fn.ty.base.kind != TY_FUNC) {
		failTok(fn.tok, "not a function")
	}

	var ty *Type
	if fn.ty.kind == TY_FUNC {
		ty = fn.ty
	} else {
		ty = fn.ty.base
	}
	paramty := ty.params

	head := Node{}
	cur := &head

	for !tok.equal(")") {
		if cur != &head {
			tok = tok.skip(",")
		}

		arg := assign(&tok, tok)
		arg.addType()

		if paramty == nil && !ty.isVariadic {
			failTok(tok, "too many arguments")
		}

		if paramty != nil {
			if paramty.kind != TY_STRUCT && paramty.kind != TY_UNION {
				arg = NewCast(arg, paramty)
			}
			paramty = paramty.next
		} else if arg.ty.kind == TY_FLOAT {
			// If parameter type is omitted (e.g. in "..."), float
			// arguments are promoted to double.
			arg = NewCast(arg, tyDouble)
		}

		cur.next = arg
		cur = cur.next
	}

	if paramty != nil {
		failTok(tok, "too few arguments")
	}

	*rest = tok.skip(")")

	node := NewUnary(ND_FUNCALL, fn, tok)
	node.functy = ty
	node.ty = ty.returnTy
	node.args = head.next

	// If a function returns a struct, it is caller's responsibility
	// to allocate a space for the return value.
	if node.ty.kind == TY_STRUCT || node.ty.kind == TY_UNION {
		node.retBuffer = NewLVar("", node.ty)
	}
	return node
}

// primary = "(" "{" stmt+ "}" ")"
// | "(" expr ")"
// | "sizeof" "(" type-name ")"
// | "sizeof" unary
// | "_Alignof" "(" type-name ")"
// | "_Alignof" unary
// | "__builtin_reg_class" "(" type-name ")"
// | ident
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
		return NewULong(int64(ty.size), start)
	}

	if tok.equal("sizeof") {
		node := unary(rest, tok.next)
		node.addType()
		return NewULong(int64(node.ty.size), tok)
	}

	if tok.equal("_Alignof") && tok.next.equal("(") && isTypename(tok.next.next) {
		ty := typename(&tok, tok.next.next)
		*rest = tok.skip(")")
		return NewULong(int64(ty.align), tok)
	}

	if tok.equal("_Alignof") {
		node := unary(rest, tok.next)
		node.addType()
		return NewULong(int64(node.ty.align), tok)
	}

	if ArchName != "riscv" {
		if tok.equal("__builtin_reg_class") {
			tok = tok.next.skip("(")
			ty := typename(&tok, tok)
			*rest = tok.skip(")")

			if ty.isInteger() || ty.kind == TY_PTR {
				return NewNum(0, start)
			}
			if ty.isFlonum() {
				return NewNum(1, start)
			}
			return NewNum(2, start)
		}
	}

	if tok.kind == TK_IDENT {
		// Variable or enum constant
		sc := findVar(tok)
		*rest = tok.next

		if sc != nil {
			if sc.vara != nil {
				return NewVarNode(sc.vara, tok)
			}
			if sc.enumty != nil {
				return NewNum(int64(sc.enumval), tok)
			}
		}

		if tok.next.equal("(") {
			failTok(tok, "implicit declaration of a function")
		}
		failTok(tok, "undefined variable")
	}

	if tok.kind == TK_STR {
		vara := newStringLiteral(tok.str, tok.ty)
		*rest = tok.next
		return NewVarNode(vara, tok)
	}

	if tok.kind == TK_NUM {
		var node *Node
		if tok.ty.isFlonum() {
			node = NewNode(ND_NUM, tok)
			node.fval = tok.fval
		} else {
			node = NewNum(tok.val, tok)
		}

		node.ty = tok.ty
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
		if ty.name == nil {
			failTok(ty.namePos, "typedef name omitted")
		}
		pushScope(getIdent(ty.name)).typedef = ty
	}
	return tok
}

func createParamLvars(param *Type) {
	if param != nil {
		createParamLvars(param.next)
		if param.name == nil {
			failTok(param.namePos, "parameter name omitted")
		}
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
	if ty.name == nil {
		failTok(ty.namePos, "function name omitted")
	}

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

	// A buffer for a struct/union return value is passed
	// as the hidden first parameter.
	rty := ty.returnTy
	if (rty.kind == TY_STRUCT || rty.kind == TY_UNION) && rty.size > 16 {
		NewLVar("", pointerTo(rty))
	}

	fn.params = locals
	if ty.isVariadic {
		switch ArchName {
		case "x64":
			fn.vaArea = NewLVar("__va_area__", arrayOf(tyChar, 136))
		case "riscv":
			fn.vaArea = NewLVar("__va_area__", arrayOf(tyChar, 64))
		default:
			fail("invalid arch:", ArchName)
		}
	}

	tok = tok.skip("{")
	// [https://www.sigbus.info/n1570#6.4.2.2p1] "__func__" is
	// automatically defined as a local variable containing the
	// current function name.
	pushScope("__func__").vara = newStringLiteral(fn.name+"\x00", arrayOf(tyChar, len(fn.name)+1))

	// [GNU] __FUNCTION__ is yet another name of __func__.
	pushScope("__FUNCTION__").vara = newStringLiteral(fn.name+"\x00", arrayOf(tyChar, len(fn.name)+1))

	fn.body = compoundStmt(&tok, tok)
	fn.locals = locals
	leaveScope()
	resolveGotoLabels()
	return tok
}

func globalVariable(tok *Token, basety *Type, attr *VarAttr) *Token {
	first := true

	for !consume(&tok, tok, ";") {
		if !first {
			tok = tok.skip(",")
		}
		first = false

		ty := declarator(&tok, tok, basety)
		if ty.name == nil {
			failTok(ty.namePos, "variable name omitted")
		}
		vara := NewGVar(getIdent(ty.name), ty)
		vara.isDefinition = !attr.isExtern
		vara.isStatic = attr.isStatic
		if attr.align != 0 {
			vara.align = attr.align
		}

		if tok.equal("=") {
			gvarInitializer(&tok, tok.next, vara)
		}
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
		tok = globalVariable(tok, basety, &attr)
	}

	return globals
}
