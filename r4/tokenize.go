package main

import (
	"fmt"
	"os"
	"strconv"
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

// Ensure that the current token is TK_NUM.
func (tok Token) getNumber() int {
	if tok.kind != TK_NUM {
		failTok(&tok, "expected a number")
	}
	return tok.val
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
		if input[p] == '+' || input[p] == '-' {
			cur.next = NewToken(TK_PUNCT, p, 1, input[p:p+1])
			cur = cur.next
			p++
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
		fmt.Fprintf(os.Stderr, "%s: invalid argument convert to number\n", s[start:pos])
		os.Exit(1)
	}
	return num, pos
}
