package main

import (
	"path/filepath"
	"strings"
)

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
	convertKeywords(tok)
	return tok
}
