package main

import (
	"fmt"
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

// Double-quote a given string and returns it.
func quotaString(str string) string {
	bufsize := 3
	for i := 0; str[i] != '\x00'; i++ {
		if str[i] == '\\' || str[i] == '"' {
			bufsize++
		}
		bufsize++
	}

	buf := make([]byte, bufsize)
	p := 0
	buf[p] = '"'
	p++
	for i := 0; str[i] != '\x00'; i++ {
		if str[i] == '\\' || str[i] == '"' {
			buf[p] = '\\'
			p++
		}
		buf[p] = str[i]
		p++
	}
	buf[p] = '"'
	p++
	buf[p] = '\x00'
	return string(buf)
}

func newStrToken(str string, tmpl *Token) *Token {
	buf := quotaString(str)
	return tokenize(newFile(tmpl.file.name, tmpl.file.fileno, buf))
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

func newNumToken(val int, tmpl *Token) *Token {
	buf := fmt.Sprintf("%d\n\x00", val)
	return tokenize(newFile(tmpl.file.name, tmpl.file.fileno, buf))
}

func readConstExpr(rest **Token, tok *Token) *Token {
	tok = copyLine(rest, tok)

	head := Token{}
	cur := &head

	for tok.kind != TK_EOF {
		// "defined(foo)" or "defined foo" becomes "1" if macro "foo"
		// is defined. Otherwise "0".
		if tok.equal("defined") {
			start := tok
			hasParen := consume(&tok, tok.next, "(")

			if tok.kind != TK_IDENT {
				failTok(start, "macro name must be an identifier")
			}
			m := findMacro(tok)
			tok = tok.next

			if hasParen {
				tok = tok.skip(")")
			}

			if m != nil {
				cur.next = newNumToken(1, start)
			} else {
				cur.next = newNumToken(0, start)
			}
			cur = cur.next
			continue
		}

		cur.next = tok
		cur = cur.next
		tok = tok.next
	}

	cur.next = tok
	return head.next
}

// Read and evaluate a constant expression.
func evalConstExpr(rest **Token, tok *Token) int64 {
	start := tok
	expr := readConstExpr(rest, tok.next)
	expr = preprocess2(expr)

	if expr.kind == TK_EOF {
		failTok(start, "no expression")
	}

	// [https://www.sigbus.info/n1570#6.10.1p4] The standard requires
	// we replace remaining non-macro identifiers with "0" before
	// evaluating a constant expression. For example, `#if foo` is
	// equivalent to `#if 0` if foo is not defined.
	for t := expr; t.kind != TK_EOF; t = t.next {
		if t.kind == TK_IDENT {
			next := t.next
			*t = *newNumToken(0, t)
			t.next = next
		}
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

// Concatenates all tokens in `tok` and returns a new string.
func joinTokens(tok, end *Token) string {
	// Compute the length of the resulting token.
	l := 1
	for t := tok; t != end && t.kind != TK_EOF; t = t.next {
		if t != tok && t.hasSpace {
			l++
		}
		l += t.len
	}

	buf := make([]byte, l)

	// Copy token texts.
	pos := 0
	for t := tok; t != end && t.kind != TK_EOF; t = t.next {
		if t != tok && t.hasSpace {
			buf[pos] = ' '
			pos++
		}
		copy(buf[pos:], []byte(t.lexeme))
		pos += t.len
	}
	buf[pos] = '\x00'
	return string(buf)
}

// Concatenates all tokens in `arg` and returns a new string token.
// This function is used for the stringizing operator (#).
func stringize(hash *Token, arg *Token) *Token {
	// Create a new string token. We need to set some value to its
	// source location for error reporting function, so we use a macro
	// name token as a template.
	s := joinTokens(arg, nil)
	return newStrToken(s, hash)
}

// Concatenate two tokens to create a new token.
func paste(lhs, rhs *Token) *Token {
	// Paste the two tokens.
	buf := fmt.Sprintf("%.*s%.*s\x00", lhs.len, lhs.lexeme, rhs.len, rhs.lexeme)

	// Tokenize the resulting string.
	tok := tokenize(newFile(lhs.file.name, lhs.file.fileno, buf))
	if tok.next.kind != TK_EOF {
		failTok(lhs, "pasting forms '%s', an invalid token", buf)
	}
	return tok
}

// Replace func-like macro parameters with given arguments.
func subst(tok *Token, args *MacroArg) *Token {
	head := Token{}
	cur := &head

	for tok.kind != TK_EOF {
		// "#" followed by a parameter is replaced with stringized actuals.
		if tok.equal("#") {
			arg := findArg(args, tok.next)
			if arg == nil {
				failTok(tok.next, "'#' is not followed by a macro parameter")
			}
			cur.next = stringize(tok, arg.tok)
			cur = cur.next
			tok = tok.next.next
			continue
		}

		if tok.equal("##") {
			if cur == &head {
				failTok(tok, "'##' cannot appear at start of macro expansion")
			}

			if tok.next.kind == TK_EOF {
				failTok(tok, "'##' cannot appear at end of macro expansion")
			}

			arg := findArg(args, tok.next)
			if arg != nil {
				if arg.tok.kind != TK_EOF {
					*cur = *paste(cur, arg.tok)
					for t := arg.tok.next; t.kind != TK_EOF; t = t.next {
						cur.next = copyToken(t)
						cur = cur.next
					}
				}
				tok = tok.next.next
				continue
			}

			*cur = *paste(cur, tok.next)
			tok = tok.next.next
			continue
		}

		arg := findArg(args, tok)

		if arg != nil && tok.next.equal("##") {
			rhs := tok.next.next

			if arg.tok.kind == TK_EOF {
				arg2 := findArg(args, rhs)
				if arg2 != nil {
					for t := arg2.tok; t.kind != TK_EOF; t = t.next {
						cur.next = copyToken(t)
						cur = cur.next
					}
				} else {
					cur.next = copyToken(rhs)
					cur = cur.next
				}
				tok = rhs.next
				continue
			}

			for t := arg.tok; t.kind != TK_EOF; t = t.next {
				cur.next = copyToken(t)
				cur = cur.next
			}
			tok = tok.next
			continue
		}

		// Handle a macro token. Macro arguments are completely macro-expanded
		// before they are substituted into a macro body.
		if arg != nil {
			t := preprocess2(arg.tok)
			t.atBol = tok.atBol
			t.hasSpace = tok.hasSpace
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
		(*rest).atBol = tok.atBol
		(*rest).hasSpace = tok.hasSpace
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
	(*rest).atBol = macroToken.atBol
	(*rest).hasSpace = macroToken.hasSpace
	return true
}

// Read an #include argument.
func readIncludeFilename(rest **Token, tok *Token, isDquote *bool) string {
	// Pattern 1: #include "foo.h"
	if tok.kind == TK_STR {
		// A double-quoted filename for #include is a special kind of
		// token, and we don't want to interpret any escape sequences in it.
		// For example, "\f" in "C:\foo" is not a formfeed character but
		// just two non-control characters, backslash and f.
		// So we don't want to use token->str.
		*isDquote = true
		*rest = skipLine(tok.next)
		return tok.lexeme[1 : tok.len-1]
	}

	// Pattern 2: #include <foo.h>
	if tok.equal("<") {
		// Reconstruct a filename from a sequence of tokens between
		// "<" and ">".
		start := tok

		// Find closing ">".
		for ; !tok.equal(">"); tok = tok.next {
			if tok.atBol || tok.kind == TK_EOF {
				failTok(tok, "expected '>'")
			}
		}

		*isDquote = false
		*rest = skipLine(tok.next)
		return strings.Trim(joinTokens(start.next, tok), "\x00")
	}

	// Pattern 3: #include FOO
	// In this case FOO must be macro-expanded to either
	// a single string token or a sequence of "<" ... ">".
	if tok.kind == TK_IDENT {
		tok2 := preprocess2(copyLine(rest, tok))
		return readIncludeFilename(&tok2, tok2, isDquote)
	}

	failTok(tok, "expected a filename")
	return ""
}

func includeFile(tok *Token, path string, filenameTok *Token) *Token {
	tok2 := tokenizeFile(path)
	if tok2 == nil {
		failTok(filenameTok, "cannot open file: %s", path)
	}
	return tok2.append(tok)
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
			var isDquote bool
			filename := readIncludeFilename(&tok, tok.next, &isDquote)

			if filename[0] != '/' {
				path := filepath.Join(filepath.Dir(start.file.name), filename)
				if fileExists(path) {
					tok = includeFile(tok, path, start.next.next)
					continue
				}
			}
			// TODO: Search a file from the include paths.
			tok = includeFile(tok, filename, start.next.next)
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
