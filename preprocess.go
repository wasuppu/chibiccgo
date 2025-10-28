package main

import (
	"path/filepath"
	"strings"
)

var condIncl *CondIncl
var macros *Macro

type MacroParam struct {
	next *MacroParam
	name string
}

type MacroArg struct {
	next *MacroArg
	name string
	tok  *Token
}

type Hideset struct {
	next *Hideset
	name string
}

type Macro struct {
	next      *Macro
	name      string
	isObjlike bool // Object-like or function-like
	params    *MacroParam
	body      *Token
	deleted   bool
}

type Ctx int

const (
	IN_THEN Ctx = iota
	IN_ELIF
	IN_ELSE
)

// `#if` can be nested, so we use a stack to manage nested `#if`s.
type CondIncl struct {
	next     *CondIncl
	ctx      Ctx
	tok      *Token
	included bool
}

func isHash(tok *Token) bool {
	return tok.atBol && tok.equal("#")
}

// Some preprocessor directives such as #include allow extraneous
// tokens before newline. This function skips such tokens.
func skipLine(tok *Token) *Token {
	if tok.atBol {
		return tok
	}
	warnTok(tok, "extra token")
	for tok.atBol {
		tok = tok.next
	}
	return tok
}

func copyToken(tok *Token) *Token {
	t := &Token{}
	*t = *tok
	t.next = nil
	return t
}

func newEof(tok *Token) *Token {
	t := copyToken(tok)
	t.kind = TK_EOF
	t.len = 0
	return t
}

func newHideset(name string) *Hideset {
	return &Hideset{
		name: name,
	}
}

func hidesetUnion(hs1, hs2 *Hideset) *Hideset {
	head := Hideset{}
	cur := &head

	for ; hs1 != nil; hs1 = hs1.next {
		cur.next = newHideset(hs1.name)
		cur = cur.next
	}
	cur.next = hs2
	return head.next
}

func hidesetContains(hs *Hideset, s string, l int) bool {
	for ; hs != nil; hs = hs.next {
		if len(hs.name) == l && hs.name[:l] == s[:l] {
			return true
		}
	}
	return false
}

func hidesetIntersection(hs1, hs2 *Hideset) *Hideset {
	head := Hideset{}
	cur := &head

	for ; hs1 != nil; hs1 = hs1.next {
		if hidesetContains(hs2, hs1.name, len(hs1.name)) {
			cur.next = newHideset(hs1.name)
			cur = cur.next
		}
	}
	return head.next
}

func addHideset(tok *Token, hs *Hideset) *Token {
	head := Token{}
	cur := &head

	for ; tok != nil; tok = tok.next {
		t := copyToken(tok)
		t.hideset = hidesetUnion(t.hideset, hs)
		cur.next = t
		cur = cur.next
	}

	return head.next
}

// Append tok2 to the end of tok1.
func (tok1 *Token) append(tok2 *Token) *Token {
	if tok1.kind == TK_EOF {
		return tok2
	}

	head := Token{}
	cur := &head

	for ; tok1.kind != TK_EOF; tok1 = tok1.next {
		cur.next = copyToken(tok1)
		cur = cur.next
	}

	cur.next = tok2
	return head.next
}

func skipCondIncl2(tok *Token) *Token {
	for tok.kind != TK_EOF {
		if isHash(tok) && (tok.next.equal("if") || tok.next.equal("ifdef") || tok.next.equal("ifndef")) {
			tok = skipCondIncl2(tok.next.next)
			continue
		}
		if isHash(tok) && tok.next.equal("endif") {
			return tok.next.next
		}
		tok = tok.next
	}
	return tok
}

// Skip until next `#else`, `#elif` or `#endif`.
// Nested `#if` and `#endif` are skipped.
func skipCondIncl(tok *Token) *Token {
	for tok.kind != TK_EOF {
		if isHash(tok) && (tok.next.equal("if") || tok.next.equal("ifdef") || tok.next.equal("ifndef")) {
			tok = skipCondIncl2(tok.next.next)
			continue
		}
		if isHash(tok) && (tok.next.equal("elif") || tok.next.equal("else") || tok.next.equal("endif")) {
			break
		}
		tok = tok.next
	}
	return tok
}

// Copy all tokens until the next newline, terminate them with
// an EOF token and then returns them. This function is used to
// create a new list of tokens for `#if` arguments.
func copyLine(rest **Token, tok *Token) *Token {
	head := Token{}
	cur := &head

	for ; !tok.atBol; tok = tok.next {
		cur.next = copyToken(tok)
		cur = cur.next
	}

	cur.next = newEof(tok)
	*rest = tok
	return head.next
}

// Read and evaluate a constant expression.
func evalConstExpr(rest **Token, tok *Token) int64 {
	start := tok
	expr := copyLine(rest, tok.next)
	expr = preprocess2(expr)

	if expr.kind == TK_EOF {
		failTok(start, "no expression")
	}

	var rest2 *Token
	val := constExpr(&rest2, expr)
	if rest2.kind != TK_EOF {
		failTok(rest2, "extra token")
	}
	return val
}

func pushCondIncl(tok *Token, included bool) *CondIncl {
	ci := &CondIncl{
		next:     condIncl,
		ctx:      IN_THEN,
		tok:      tok,
		included: included,
	}
	condIncl = ci
	return ci
}

func findMacro(tok *Token) *Macro {
	if tok.kind != TK_IDENT {
		return nil
	}

	for m := macros; m != nil; m = m.next {
		if len(m.name) == tok.len && m.name == tok.lexeme {
			if m.deleted {
				return nil
			} else {
				return m
			}
		}
	}
	return nil
}

func readMacroParams(rest **Token, tok *Token) *MacroParam {
	head := MacroParam{}
	cur := &head

	for !tok.equal(")") {
		if cur != &head {
			tok = tok.skip(",")
		}

		if tok.kind != TK_IDENT {
			failTok(tok, "expected an identifier")
		}

		m := &MacroParam{name: tok.lexeme}
		cur.next = m
		cur = cur.next
		tok = tok.next
	}
	*rest = tok.next
	return head.next
}

func addMacro(name string, isObjlike bool, body *Token) *Macro {
	m := &Macro{
		next:      macros,
		name:      name,
		isObjlike: isObjlike,
		body:      body,
	}
	macros = m
	return m
}

func readMacroDefinition(rest **Token, tok *Token) {
	if tok.kind != TK_IDENT {
		failTok(tok, "macro name must be an identifier")
	}
	name := tok.lexeme
	tok = tok.next

	if !tok.hasSpace && tok.equal("(") {
		// Function-like macro
		params := readMacroParams(&tok, tok.next)
		m := addMacro(name, false, copyLine(rest, tok))
		m.params = params
	} else {
		// Object-like macro
		addMacro(name, true, copyLine(rest, tok))
	}
}

func readMacroArgOne(rest **Token, tok *Token) *MacroArg {
	head := Token{}
	cur := &head
	level := 0

	for level > 0 || (!tok.equal(",") && !tok.equal(")")) {
		if tok.kind == TK_EOF {
			failTok(tok, "premature end of input")
		}

		if tok.equal("(") {
			level++
		} else if tok.equal(")") {
			level--
		}

		cur.next = copyToken(tok)
		cur = cur.next
		tok = tok.next
	}

	cur.next = newEof(tok)

	arg := &MacroArg{tok: head.next}
	*rest = tok
	return arg
}

func readMacroArgs(rest **Token, tok *Token, params *MacroParam) *MacroArg {
	start := tok
	tok = tok.next.next

	head := MacroArg{}
	cur := &head

	pp := params
	for ; pp != nil; pp = pp.next {
		if cur != &head {
			tok = tok.skip(",")
		}
		cur.next = readMacroArgOne(&tok, tok)
		cur = cur.next
		cur.name = pp.name
	}

	if pp != nil {
		failTok(start, "too many arguments")
	}
	tok.skip(")")
	*rest = tok
	return head.next
}

func findArg(args *MacroArg, tok *Token) *MacroArg {
	for ap := args; ap != nil; ap = ap.next {
		if tok.len == len(ap.name) && tok.lexeme == ap.name {
			return ap
		}
	}
	return nil
}

// Replace func-like macro parameters with given arguments.
func subst(tok *Token, args *MacroArg) *Token {
	head := Token{}
	cur := &head

	for tok.kind != TK_EOF {
		arg := findArg(args, tok)

		// Handle a macro token. Macro arguments are completely macro-expanded
		// before they are substituted into a macro body.
		if arg != nil {
			t := preprocess2(arg.tok)
			for ; t.kind != TK_EOF; t = t.next {
				cur.next = copyToken(t)
				cur = cur.next
			}
			tok = tok.next
			continue
		}

		// Handle a non-macro token.
		cur.next = copyToken(tok)
		cur = cur.next
		tok = tok.next
		continue
	}

	cur.next = tok
	return head.next
}

// If tok is a macro, expand it and return true.
// Otherwise, do nothing and return false.
func expandMacro(rest **Token, tok *Token) bool {
	if hidesetContains(tok.hideset, tok.lexeme, tok.len) {
		return false
	}

	m := findMacro(tok)
	if m == nil {
		return false
	}

	// Object-like macro application
	if m.isObjlike {
		hs := hidesetUnion(tok.hideset, newHideset(m.name))
		body := addHideset(m.body, hs)
		*rest = body.append(tok.next)
		return true
	}

	// If a funclike macro token is not followed by an argument list,
	// treat it as a normal identifier.
	if !tok.next.equal("(") {
		return false
	}

	// Function-like macro application
	macroToken := tok
	args := readMacroArgs(&tok, tok, m.params)
	rparen := tok

	hs := hidesetIntersection(macroToken.hideset, rparen.hideset)
	hs = hidesetUnion(hs, newHideset(m.name))

	body := subst(m.body, args)
	body = addHideset(body, hs)
	*rest = body.append(tok.next)
	return true
}

// Visit all tokens in `tok` while evaluating preprocessing
// macros and directives.
func preprocess2(tok *Token) *Token {
	head := Token{}
	cur := &head

	for tok.kind != TK_EOF {
		// If it is a macro, expand it.
		if expandMacro(&tok, tok) {
			continue
		}

		// Pass through if it is not a "#".
		if !isHash(tok) {
			cur.next = tok
			cur = cur.next
			tok = tok.next
			continue
		}

		start := tok
		tok = tok.next

		if tok.equal("include") {
			tok = tok.next

			if tok.kind != TK_STR {
				failTok(tok, "expected a filename")
			}

			var path string
			if tok.str[0] == '/' {
				path = strings.Trim(tok.str, string('\x00'))
			} else {
				path = filepath.Join(filepath.Dir(tok.file.name), strings.Trim(tok.str, string('\x00')))
			}

			tok2 := tokenizeFile(path)

			if tok2 == nil {
				failTok(tok, "fail to tokenize %s", path)
			}
			tok = skipLine(tok.next)

			tok = tok2.append(tok)
			continue
		}

		if tok.equal("define") {
			readMacroDefinition(&tok, tok.next)
			continue
		}

		if tok.equal("undef") {
			tok = tok.next
			if tok.kind != TK_IDENT {
				failTok(tok, "macro name must be an identifier")
			}
			name := tok.lexeme
			tok = skipLine(tok.next)

			m := addMacro(name, true, nil)
			m.deleted = true
			continue
		}

		if tok.equal("if") {
			val := evalConstExpr(&tok, tok)
			if val == 0 {
				pushCondIncl(start, false)
				tok = skipCondIncl(tok)
			} else {
				pushCondIncl(start, true)
			}
			continue
		}

		if tok.equal("else") {
			if condIncl == nil || condIncl.ctx == IN_ELSE {
				failTok(start, "stray #else")
			}
			condIncl.ctx = IN_ELSE
			tok = skipLine(tok.next)

			if condIncl.included {
				tok = skipCondIncl(tok)
			}
			continue
		}

		if tok.equal("ifdef") {
			var defined bool
			if findMacro(tok.next) != nil {
				defined = true
			}
			pushCondIncl(tok, defined)
			tok = skipLine(tok.next.next)
			if !defined {
				tok = skipCondIncl(tok)
			}
			continue
		}

		if tok.equal("ifndef") {
			var defined bool
			if findMacro(tok.next) != nil {
				defined = true
			}
			pushCondIncl(tok, !defined)
			tok = skipLine(tok.next.next)
			if defined {
				tok = skipCondIncl(tok)
			}
			continue
		}

		if tok.equal("elif") {
			if condIncl == nil || condIncl.ctx == IN_ELSE {
				failTok(start, "stray #elif")
			}
			condIncl.ctx = IN_ELIF

			if !condIncl.included && evalConstExpr(&tok, tok) != 0 {
				condIncl.included = true
			} else {
				tok = skipCondIncl(tok)
			}
			continue
		}

		if tok.equal("endif") {
			if condIncl == nil {
				failTok(start, "stray #endif")
			}
			condIncl = condIncl.next
			tok = skipLine(tok.next)
			continue
		}

		// `#`-only line is legal. It's called a null directive.
		if tok.atBol {
			continue
		}

		failTok(tok, "invalid preprocessor directive")
	}

	cur.next = tok
	return head.next
}

// Entry point function of the preprocessor.
func preprocess(tok *Token) *Token {
	tok = preprocess2(tok)
	if condIncl != nil {
		failTok(condIncl.tok, "unterminated conditional directive")
	}
	convertKeywords(tok)
	return tok
}
