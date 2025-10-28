package main

import (
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
	"unicode"
)

var source string
var currentInputLoc int

var kws = []string{"return", "if", "else", "for", "while", "int", "sizeof"}

// Token
type TokenKind int

const (
	TK_IDENT   TokenKind = iota // Identifiers
	TK_PUNCT                    // Punctuators
	TK_KEYWORD                  // Keywords
	TK_NUM                      // Numeric literals
	TK_EOF                      // End-of-file markers
)

// Token type
type Token struct {
	kind   TokenKind // Token kind
	next   *Token    // Next token
	val    int       // If kind is TK_NUM, its value
	loc    int       // Token location
	len    int       // Token length
	lexeme string    // Token lexeme value in string
}

// Create a new token.
func NewToken(kind TokenKind, pos int, len int, lexme string) *Token {
	return &Token{
		kind:   kind,
		loc:    pos,
		len:    len,
		lexeme: lexme,
	}
}

// Consumes the current token if it matches `op`.
func (tok Token) equal(op string) bool {
	return len(op) == tok.len && tok.lexeme == op
}

// Ensure that the current token is `op`.
func (tok Token) skip(op string) *Token {
	if !tok.equal(op) {
		failTok(&tok, "expected '%s'", op)
	}
	return tok.next
}

func consume(rest **Token, tok *Token, str string) bool {
	if tok.equal(str) {
		*rest = tok.next
		return true
	}
	*rest = tok
	return false
}

// Returns true if c is valid as the first character of an identifier.
func isIdent1(c byte) bool {
	return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'
}

// Returns true if c is valid as a non-first character of an identifier.
func isIdent2(c byte) bool {
	return isIdent1(c) || ('0' <= c && c <= '9')
}

// Read a punctuator token from p and returns its length.
func readPunct(p int) int {
	if strings.HasPrefix(source[p:], "==") || strings.HasPrefix(source[p:], "!=") ||
		strings.HasPrefix(source[p:], "<=") || strings.HasPrefix(source[p:], ">=") {
		return 2
	}

	if unicode.IsPunct(rune(source[p])) || unicode.IsSymbol(rune(source[p])) {
		return 1
	} else {
		return 0
	}
}

func isKeyword(tok *Token) bool {
	return slices.ContainsFunc(kws, tok.equal)
}

func convertKeywords(tok *Token) {
	for t := tok; t.kind != TK_EOF; t = t.next {
		if isKeyword(t) {
			t.kind = TK_KEYWORD
		}
	}
}

// Tokenize a given string and returns new tokens.
func tokenize(input string) *Token {
	source = input
	head := Token{}
	cur := &head
	p := 0

	for p < len(input) {
		// Skip whitespace characters.
		if unicode.IsSpace(rune(input[p])) {
			p++
			continue
		}

		// Numeric literal
		if unicode.IsDigit(rune(input[p])) {
			n, np := parseNumber(input, p)
			cur.next = NewToken(TK_NUM, p, np-p, input[p:np])
			cur = cur.next
			cur.val = n
			p = np
			continue
		}

		// Identifier or keyword
		if isIdent1(input[p]) {
			start := p
			for {
				p++
				if !isIdent2(input[p]) {
					break
				}
			}
			cur.next = NewToken(TK_IDENT, start, p-start, input[start:p])
			cur = cur.next
			continue
		}

		// Punctuator
		punctLen := readPunct(p)
		if punctLen != 0 {
			cur.next = NewToken(TK_PUNCT, p, punctLen, input[p:p+punctLen])
			cur = cur.next
			p += cur.len
			continue
		}

		failAt(p, "invalid token")
	}

	cur.next = NewToken(TK_EOF, p, 0, "")
	convertKeywords(head.next)
	return head.next
}

func parseNumber(s string, pos int) (int, int) {
	start := pos
	for pos < len(s) && unicode.IsDigit(rune(s[pos])) {
		pos++
	}
	num, err := strconv.Atoi(s[start:pos])
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: invalid argument convert to number: %s\n", s[start:pos], err)
		os.Exit(1)
	}
	return num, pos
}
