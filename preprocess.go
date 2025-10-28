package main

import (
	"path/filepath"
	"strings"
)

var condIncl *CondIncl

type Ctx int

const (
	IN_THEN Ctx = iota
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

// Append tok2 to the end of tok1.
func (tok1 *Token) append(tok2 *Token) *Token {
	if tok1 == nil || tok1.kind == TK_EOF {
		return tok2
	}

	head := Token{}
	cur := &head

	for ; tok1 != nil && tok1.kind != TK_EOF; tok1 = tok1.next {
		cur.next = copyToken(tok1)
		cur = cur.next
	}

	cur.next = tok2
	return head.next
}

func skipCondIncl2(tok *Token) *Token {
	for tok.kind != TK_EOF {
		if isHash(tok) && tok.next.equal("if") {
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

// Skip until next `#else` or `#endif`.
// Nested `#if` and `#endif` are skipped.
func skipCondIncl(tok *Token) *Token {
	for tok.kind != TK_EOF {
		if isHash(tok) && tok.next.equal("if") {
			tok = skipCondIncl2(tok.next.next)
			continue
		}
		if isHash(tok) && (tok.next.equal("else") || tok.next.equal("endif")) {
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

// Visit all tokens in `tok` while evaluating preprocessing
// macros and directives.
func preprocess2(tok *Token) *Token {
	head := Token{}
	cur := &head

	for tok.kind != TK_EOF {
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
