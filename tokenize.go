package main

import (
	"fmt"
	"io"
	"os"
	"slices"
	"strconv"
	"strings"
	"unicode"
)

var source string
var currentInputLoc int
var currentFilename string

var kws = []string{
	"return", "if", "else", "for", "while", "int", "sizeof", "char",
	"struct", "union", "short", "long", "void", "typedef", "_Bool",
	"enum", "static",
}

var puncts = []string{
	"==", "!=", "<=", ">=", "->", "+=", "-=", "*=", "/=", "++", "--",
}

// Token
type TokenKind int

const (
	TK_IDENT   TokenKind = iota // Identifiers
	TK_PUNCT                    // Punctuators
	TK_KEYWORD                  // Keywords
	TK_STR                      // String literals
	TK_NUM                      // Numeric literals
	TK_EOF                      // End-of-file markers
)

// Token type
type Token struct {
	kind   TokenKind // Token kind
	next   *Token    // Next token
	val    int64     // If kind is TK_NUM, its value
	loc    int       // Token location
	len    int       // Token length
	lexeme string    // Token lexeme value in string
	ty     *Type     // Used if TK_STR
	str    string    // String literal contents including terminating '\0'
	lineno int       // Line number
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

func fromHex(c byte) byte {
	if '0' <= c && c <= '9' {
		return c - '0'
	}
	if 'a' <= c && c <= 'f' {
		return c - 'a' + 10
	}
	return c - 'A' + 10
}

// Read a punctuator token from p and returns its length.
func readPunct(p int) int {
	for i := range puncts {
		if strings.HasPrefix(source[p:], puncts[i]) {
			return len(puncts[i])
		}
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

func readEscapedChar(newPos *int, p int) byte {
	if '0' <= source[p] && source[p] <= '7' {
		// Read an octal number.
		c := source[p] - '0'
		p++
		if '0' <= source[p] && source[p] <= '7' {
			c = (c << 3) + (source[p] - '0')
			p++
			if '0' <= source[p] && source[p] <= '7' {
				c = (c << 3) + (source[p] - '0')
				p++
			}
		}
		*newPos = p
		return c
	}

	if source[p] == 'x' {
		// Read a hexadecimal number.
		p++
		if !isXDigit(source[p]) {
			failAt(p, "invalid hex escape sequence")
		}

		c := byte(0)
		for ; isXDigit(source[p]); p++ {
			c = (c << 4) + fromHex(source[p])
		}
		*newPos = p
		return c
	}

	*newPos = p + 1

	switch source[p] {
	case 'a':
		return '\a'
	case 'b':
		return '\b'
	case 't':
		return '\t'
	case 'n':
		return '\n'
	case 'v':
		return '\v'
	case 'f':
		return '\f'
	case 'r':
		return '\r'
	// [GNU] \e for the ASCII escape character is a GNU C extension.
	case 'e':
		return 27
	default:
		return source[p]
	}
}

// Find a closing double-quote.
func stringLiteralEnd(p int) int {
	start := p
	sourceLen := len(source)
	for ; source[p] != '"'; p++ {
		if source[p] == '\n' || p == sourceLen {
			failAt(start, "unclosed string literal")
		}
		if source[p] == '\\' {
			p++
		}
	}
	return p
}

func readStringLiteral(start int) *Token {
	end := stringLiteralEnd(start + 1)
	buf := make([]byte, end-start)
	len := 0

	for p := start + 1; p < end; {
		if source[p] == '\\' {
			buf[len] = readEscapedChar(&p, p+1)
			len++
		} else {
			buf[len] = source[p]
			len++
			p++
		}
	}

	tok := NewToken(TK_STR, start, end+1-start, source[start:end+1])
	tok.ty = arrayOf(tyChar, len+1)
	tok.str = fmt.Sprintf("%s\x00", buf)
	return tok
}

func readCharLiteral(start int) *Token {
	p := start + 1
	if p == len(source) {
		failAt(start, "unclosed char literal")
	}

	var c byte
	if source[p] == '\\' {
		c = readEscapedChar(&p, p+1)
	} else {
		c = source[p]
		p++
	}

	end := strings.Index(source[p:], "'")
	if end == -1 {
		failAt(p, "unclosed char literal")
	}

	tok := NewToken(TK_NUM, start, p+end-start+1, source[start:p+end+1])
	tok.val = int64(int8(c))
	return tok
}

func readIntLiteral(start int) *Token {
	p := start

	base := 10
	if strings.HasPrefix(strings.ToLower(source[p:p+2]), "0x") && isXDigit(source[p+2]) {
		p += 2
		base = 16
	} else if strings.HasPrefix(strings.ToLower(source[p:p+2]), "0b") && isXDigit(source[p+2]) {
		p += 2
		base = 2
	} else if source[p] == '0' {
		base = 8
	}

	end := getIntegerEnd(source[p:])
	val, err := strconv.ParseInt(source[p:p+end], base, 64)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: invalid argument convert to number: %s\n", source[start:p+end], err)
		os.Exit(1)
	}

	p += end

	if unicode.IsDigit(rune(source[p])) || unicode.IsLetter(rune(source[p])) {
		failAt(p, "invalid digit")
	}

	tok := NewToken(TK_NUM, start, p-start, source[start:p])
	tok.val = val
	return tok
}

func convertKeywords(tok *Token) {
	for t := tok; t.kind != TK_EOF; t = t.next {
		if isKeyword(t) {
			t.kind = TK_KEYWORD
		}
	}
}

// Initialize line info for all tokens.
func addLineNumbers(tok *Token) {
	p := currentInputLoc
	n := 1

	for {
		if p == tok.loc {
			tok.lineno = n
			tok = tok.next
		}
		if source[p] == '\n' {
			n++
		}

		p++
		if p >= len(source) {
			break
		}
	}
}

// Tokenize a given string and returns new tokens.
func tokenize(filename string, input string) *Token {
	currentFilename = filename
	source = input
	head := Token{}
	cur := &head
	p := 0

	for p < len(input) {
		// Skip line comments.
		if strings.HasPrefix(input[p:], "//") {
			p += 2
			for input[p] != '\n' {
				p++
			}
			continue
		}

		// Skip block comments.
		if strings.HasPrefix(input[p:], "/*") {
			q := strings.Index(input[p+2:], "*/")
			if q == -1 {
				failAt(p, "unclosed block comment")
			}
			p = p + 2 + q + 2
			continue
		}

		// Skip whitespace characters.
		if unicode.IsSpace(rune(input[p])) {
			p++
			continue
		}

		// Numeric literal
		if unicode.IsDigit(rune(input[p])) {
			cur.next = readIntLiteral(p)
			cur = cur.next
			p += cur.len
			continue
		}

		// String literal
		if input[p] == '"' {
			cur.next = readStringLiteral(p)
			cur = cur.next
			p += cur.len
			continue
		}

		// Character literal
		if input[p] == '\'' {
			cur.next = readCharLiteral(p)
			cur = cur.next
			p += cur.len
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
	addLineNumbers(head.next)
	convertKeywords(head.next)
	return head.next
}

// Returns the contents of a given file.
func readFile(path string) string {
	var data []byte
	var err error

	if path == "-" {
		data, err = io.ReadAll(os.Stdin)
	} else {
		data, err = os.ReadFile(path)
	}
	if err != nil {
		fail(fmt.Sprint("fail to read source:", err))
	}

	content := string(data)
	if len(content) > 0 && content[len(content)-1] != '\n' {
		content += "\n"
	}
	return content
}

func tokenizeFile(path string) *Token {
	return tokenize(path, readFile(path))
}

func getIntegerEnd(s string) int {
	end := 0
	for end < len(s) && isXDigit(s[end]) {
		end++
	}
	return end
}

func isXDigit(c byte) bool {
	return (c >= '0' && c <= '9') ||
		(c >= 'a' && c <= 'f') ||
		(c >= 'A' && c <= 'F')
}
