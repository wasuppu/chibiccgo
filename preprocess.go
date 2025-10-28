package main

import (
	"fmt"
	"path/filepath"
	"strings"
	"time"
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

type MacroHandlerFn func(*Token) *Token

type Macro struct {
	next       *Macro
	name       string
	isObjlike  bool // Object-like or function-like
	params     *MacroParam
	isVariadic bool
	body       *Token
	deleted    bool
	handler    MacroHandlerFn
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
	buf := quotaString(str + "\x00")
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

	// Convert pp-numbers to regular numbers
	convertPPTokens(expr)

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

func readMacroParams(rest **Token, tok *Token, isVariadic *bool) *MacroParam {
	head := MacroParam{}
	cur := &head

	for !tok.equal(")") {
		if cur != &head {
			tok = tok.skip(",")
		}

		if tok.equal("...") {
			*isVariadic = true
			*rest = tok.next.skip(")")
			return head.next
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
		isVariadic := false
		params := readMacroParams(&tok, tok.next, &isVariadic)

		m := addMacro(name, false, copyLine(rest, tok))
		m.params = params
		m.isVariadic = isVariadic
	} else {
		// Object-like macro
		addMacro(name, true, copyLine(rest, tok))
	}
}

func readMacroArgOne(rest **Token, tok *Token, readRest bool) *MacroArg {
	head := Token{}
	cur := &head
	level := 0

	for {
		if level == 0 && tok.equal(")") {
			break
		}
		if level == 0 && !readRest && tok.equal(",") {
			break
		}

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

func readMacroArgs(rest **Token, tok *Token, params *MacroParam, isVariadic bool) *MacroArg {
	start := tok
	tok = tok.next.next

	head := MacroArg{}
	cur := &head

	pp := params
	for ; pp != nil; pp = pp.next {
		if cur != &head {
			tok = tok.skip(",")
		}
		cur.next = readMacroArgOne(&tok, tok, false)
		cur = cur.next
		cur.name = pp.name
	}

	if isVariadic {
		arg := &MacroArg{}
		if tok.equal(")") {
			arg.tok = newEof(tok)
		} else {
			if pp != params {
				tok = tok.skip(",")
			}
			arg = readMacroArgOne(&tok, tok, true)
		}
		arg.name = "__VA_ARGS__"
		cur.next = arg
		cur = cur.next
	} else if pp != nil {
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

	// Built-in dynamic macro application such as __LINE__
	if m.handler != nil {
		*rest = m.handler(tok)
		(*rest).next = tok.next
		return true
	}

	// Object-like macro application
	if m.isObjlike {
		hs := hidesetUnion(tok.hideset, newHideset(m.name))
		body := addHideset(m.body, hs)
		for t := body; t.kind != TK_EOF; t = t.next {
			t.origin = tok
		}
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
	args := readMacroArgs(&tok, tok, m.params, m.isVariadic)
	rparen := tok

	hs := hidesetIntersection(macroToken.hideset, rparen.hideset)
	hs = hidesetUnion(hs, newHideset(m.name))

	body := subst(m.body, args)
	body = addHideset(body, hs)
	for t := body; t.kind != TK_EOF; t = t.next {
		t.origin = macroToken
	}
	*rest = body.append(tok.next)
	(*rest).atBol = macroToken.atBol
	(*rest).hasSpace = macroToken.hasSpace
	return true
}

func searchIncludePaths(filename string) string {
	if filename[0] == '/' {
		return filename
	}

	// Search a file from the include paths.
	for i := 0; i < len(includePaths); i++ {
		path := filepath.Join(includePaths[i], filename)
		if fileExists(path) {
			return path
		}
	}

	return ""
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

			if filename[0] != '/' && isDquote {
				path := filepath.Join(filepath.Dir(start.file.name), filename)
				if fileExists(path) {
					tok = includeFile(tok, path, start.next.next)
					continue
				}
			}

			path := searchIncludePaths(filename)
			if len(path) > 0 {
				tok = includeFile(tok, path, start.next.next)
			} else {
				tok = includeFile(tok, filename, start.next.next)
			}
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
			undefMacro(tok.lexeme)
			tok = skipLine(tok.next)
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

		if tok.equal("error") {
			failTok(tok, "error")
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

func defineMacro(name, buf string) {
	tok := tokenize(newFile("<built-in>", 1, buf+"\x00"))
	addMacro(name, true, tok)
}

func undefMacro(name string) {
	m := addMacro(name, true, nil)
	m.deleted = true
}

func addBuiltin(name string, fn MacroHandlerFn) *Macro {
	m := addMacro(name, true, nil)
	m.handler = fn
	return m
}

func fileMacro(tmpl *Token) *Token {
	for tmpl.origin != nil {
		tmpl = tmpl.origin
	}
	return newStrToken(tmpl.file.name, tmpl)
}

func lineMacro(tmpl *Token) *Token {
	for tmpl.origin != nil {
		tmpl = tmpl.origin
	}
	return newNumToken(tmpl.lineno, tmpl)
}

// __COUNTER__ is expanded to serial values starting from 0.
var Counter int

func counterMacro(tmpl *Token) *Token {
	t := Counter
	Counter++
	return newNumToken(t, tmpl)
}

// __DATE__ is expanded to the current date, e.g. "May 17 2020".
func formatDate(t time.Time) string {
	months := []string{
		"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
	}
	return fmt.Sprintf("\"%s %2d %d\"",
		months[t.Month()-1], t.Day(), t.Year())
}

// __TIME__ is expanded to the current time, e.g. "13:34:03".
func formatTime(t time.Time) string {
	return fmt.Sprintf("\"%02d:%02d:%02d\"",
		t.Hour(), t.Minute(), t.Second())
}

func (a X64) initMacro() {
	// Define predefined macros
	defineMacro("_LP64", "1")
	defineMacro("__C99_MACRO_WITH_VA_ARGS", "1")
	defineMacro("__ELF__", "1")
	defineMacro("__LP64__", "1")
	defineMacro("__SIZEOF_DOUBLE__", "8")
	defineMacro("__SIZEOF_FLOAT__", "4")
	defineMacro("__SIZEOF_INT__", "4")
	defineMacro("__SIZEOF_LONG_DOUBLE__", "8")
	defineMacro("__SIZEOF_LONG_LONG__", "8")
	defineMacro("__SIZEOF_LONG__", "8")
	defineMacro("__SIZEOF_POINTER__", "8")
	defineMacro("__SIZEOF_PTRDIFF_T__", "8")
	defineMacro("__SIZEOF_SHORT__", "2")
	defineMacro("__SIZEOF_SIZE_T__", "8")
	defineMacro("__SIZE_TYPE__", "unsigned long")
	defineMacro("__STDC_HOSTED__", "1")
	defineMacro("__STDC_NO_ATOMICS__", "1")
	defineMacro("__STDC_NO_COMPLEX__", "1")
	defineMacro("__STDC_NO_THREADS__", "1")
	defineMacro("__STDC_NO_VLA__", "1")
	defineMacro("__STDC_VERSION__", "201112L")
	defineMacro("__STDC__", "1")
	defineMacro("__USER_LABEL_PREFIX__", "")
	defineMacro("__alignof__", "_Alignof")
	defineMacro("__amd64", "1")
	defineMacro("__amd64__", "1")
	defineMacro("__chibicc__", "1")
	defineMacro("__const__", "const")
	defineMacro("__gnu_linux__", "1")
	defineMacro("__inline__", "inline")
	defineMacro("__linux", "1")
	defineMacro("__linux__", "1")
	defineMacro("__signed__", "signed")
	defineMacro("__typeof__", "typeof")
	defineMacro("__unix", "1")
	defineMacro("__unix__", "1")
	defineMacro("__volatile__", "volatile")
	defineMacro("__x86_64", "1")
	defineMacro("__x86_64__", "1")
	defineMacro("linux", "1")
	defineMacro("unix", "1")

	addBuiltin("__FILE__", fileMacro)
	addBuiltin("__LINE__", lineMacro)
	addBuiltin("__COUNTER__", counterMacro)

	now := time.Now()
	defineMacro("__DATE__", formatDate(now))
	defineMacro("__TIME__", formatTime(now))
}

func (a RiscV) initMacro() {
	// Define predefined macros
	defineMacro("_LP64", "1")
	defineMacro("__C99_MACRO_WITH_VA_ARGS", "1")
	defineMacro("__ELF__", "1")
	defineMacro("__LP64__", "1")
	defineMacro("__SIZEOF_DOUBLE__", "8")
	defineMacro("__SIZEOF_FLOAT__", "4")
	defineMacro("__SIZEOF_INT__", "4")
	defineMacro("__SIZEOF_LONG_DOUBLE__", "8")
	defineMacro("__SIZEOF_LONG_LONG__", "8")
	defineMacro("__SIZEOF_LONG__", "8")
	defineMacro("__SIZEOF_POINTER__", "8")
	defineMacro("__SIZEOF_PTRDIFF_T__", "8")
	defineMacro("__SIZEOF_SHORT__", "2")
	defineMacro("__SIZEOF_SIZE_T__", "8")
	defineMacro("__SIZE_TYPE__", "unsigned long")
	defineMacro("__STDC_HOSTED__", "1")
	defineMacro("__STDC_NO_ATOMICS__", "1")
	defineMacro("__STDC_NO_COMPLEX__", "1")
	defineMacro("__STDC_NO_THREADS__", "1")
	defineMacro("__STDC_NO_VLA__", "1")
	defineMacro("__STDC_VERSION__", "201112L")
	defineMacro("__STDC__", "1")
	defineMacro("__USER_LABEL_PREFIX__", "")
	defineMacro("__alignof__", "_Alignof")
	defineMacro("__rvcc__", "1")
	defineMacro("__const__", "const")
	defineMacro("__gnu_linux__", "1")
	defineMacro("__inline__", "inline")
	defineMacro("__linux", "1")
	defineMacro("__linux__", "1")
	defineMacro("__signed__", "signed")
	defineMacro("__typeof__", "typeof")
	defineMacro("__unix", "1")
	defineMacro("__unix__", "1")
	defineMacro("__volatile__", "volatile")
	defineMacro("linux", "1")
	defineMacro("unix", "1")
	defineMacro("__riscv_mul", "1")
	defineMacro("__riscv_muldiv", "1")
	defineMacro("__riscv_fdiv", "1")
	defineMacro("__riscv_xlen", "64")
	defineMacro("__riscv", "1")
	defineMacro("__riscv64", "1")
	defineMacro("__riscv_div", "1")
	defineMacro("__riscv_float_abi_double", "1")
	defineMacro("__riscv_flen", "64")

	addBuiltin("__FILE__", fileMacro)
	addBuiltin("__LINE__", lineMacro)
}

// Concatenate adjacent string literals into a single string literal
// as per the C spec.
func joinAdjacentStringLiterals(tok1 *Token) {
	for tok1.kind != TK_EOF {
		if tok1.kind != TK_STR || tok1.next.kind != TK_STR {
			tok1 = tok1.next
			continue
		}

		tok2 := tok1.next
		for tok2.kind == TK_STR {
			tok2 = tok2.next
		}

		l := tok1.ty.arrayLen
		for t := tok1.next; t != tok2; t = t.next {
			l = l + t.ty.arrayLen - 1
		}

		buf := make([]byte, tok1.ty.base.size*l)
		i := 0
		for t := tok1; t != tok2; t = t.next {
			copy(buf[i:], t.str[:t.ty.size])
			i = i + t.ty.size - t.ty.base.size
		}

		*tok1 = *copyToken(tok1)
		tok1.ty = arrayOf(tok1.ty.base, l)
		tok1.str = string(buf)
		tok1.next = tok2
		tok1 = tok2
	}
}

// Entry point function of the preprocessor.
func preprocess(tok *Token) *Token {
	tok = preprocess2(tok)
	if condIncl != nil {
		failTok(condIncl.tok, "unterminated conditional directive")
	}
	convertPPTokens(tok)
	joinAdjacentStringLiterals(tok)

	return tok
}
