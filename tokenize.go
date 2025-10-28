package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

// Token
type TokenKind int

const (
	TK_PUNCT TokenKind = iota // PunctuatorsMore actions
	TK_NUM                    // Numeric literals
	TK_EOF                    // End-of-file markers
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

// Ensure that the current token is `s`.
func (tok Token) skip(s string) *Token {
	if !tok.equal(s) {
		failTok(&tok, "expected '%s'", s)
	}
	return tok.next
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

// Tokenize `p` and returns new tokens.
func tokenize() *Token {
	input := source
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
