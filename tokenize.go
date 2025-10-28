package main

import (
	"encoding/binary"
	"fmt"
	"io"
	"os"
	"slices"
	"strconv"
	"strings"
	"unicode"
)

var inputfiles []*File

var source string
var currentFile *File

// True if the current position is at the beginning of a line
var atBol bool

// True if the current position follows a space character
var hasSpace bool

var kws = []string{
	"return", "if", "else", "for", "while", "int", "sizeof", "char",
	"struct", "union", "short", "long", "void", "typedef", "_Bool",
	"enum", "static", "goto", "break", "continue", "switch", "case",
	"default", "extern", "_Alignof", "_Alignas", "do", "signed",
	"unsigned", "const", "volatile", "auto", "register", "restrict",
	"__restrict", "__restrict__", "_Noreturn", "float", "double",
}

var puncts = []string{
	"<<=", ">>=", "...", "==", "!=", "<=", ">=", "->", "+=",
	"-=", "*=", "/=", "++", "--", "%=", "&=", "|=", "^=", "&&",
	"||", "<<", ">>", "##",
}

type File struct {
	name     string
	fileno   int
	contents string
}

func newFile(name string, fileno int, contents string) *File {
	return &File{
		name:     name,
		fileno:   fileno,
		contents: contents,
	}
}

// Token
type TokenKind int

const (
	TK_IDENT   TokenKind = iota // Identifiers
	TK_PUNCT                    // Punctuators
	TK_KEYWORD                  // Keywords
	TK_STR                      // String literals
	TK_NUM                      // Numeric literals
	TK_PP_NUM                   // Preprocessing numbers
	TK_EOF                      // End-of-file markers
)

// Token type
type Token struct {
	kind     TokenKind // Token kind
	next     *Token    // Next token
	val      int64     // If kind is TK_NUM, its value
	fval     float64   // If kind is TK_NUM, its value
	loc      int       // Token location
	len      int       // Token length
	lexeme   string    // Token lexeme value in string
	ty       *Type     // Used if TK_NUM or TK_STR
	str      string    // String literal contents including terminating '\0'
	file     *File     // Source location
	lineno   int       // Line number
	atBol    bool      // True if this token is at beginning of line
	hasSpace bool      // True if this token follows a space character
	hideset  *Hideset  // For macro expansion
	origin   *Token    // If this is expanded from a macro, the original token
}

// Create a new token.
func NewToken(kind TokenKind, pos int, len int, lexme string) *Token {
	tok := &Token{
		kind:     kind,
		loc:      pos,
		len:      len,
		file:     currentFile,
		lexeme:   lexme,
		atBol:    atBol,
		hasSpace: hasSpace,
	}
	atBol = false
	hasSpace = false
	return tok
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

// Read an identifier and returns the length of it.
// If p does not point to a valid identifier, 0 is returned.
func readIdent(start int) int {
	p := start
	c := decodeUtf8(&p, p)
	if !isIdent1(c) {
		return 0
	}

	for {
		var q int
		c = decodeUtf8(&q, p)
		if !isIdent2(c) {
			return p - start
		}
		p = q
	}
}

func fromHex(c byte) int {
	if '0' <= c && c <= '9' {
		return int(c - '0')
	}
	if 'a' <= c && c <= 'f' {
		return int(c - 'a' + 10)
	}
	return int(c - 'A' + 10)
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

func readEscapedChar(newPos *int, p int) int {
	if '0' <= source[p] && source[p] <= '7' {
		// Read an octal number.
		c := int(source[p] - '0')
		p++
		if '0' <= source[p] && source[p] <= '7' {
			c = (c << 3) + (int(source[p]) - '0')
			p++
			if '0' <= source[p] && source[p] <= '7' {
				c = (c << 3) + (int(source[p]) - '0')
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

		c := int(0)
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
		return int(source[p])
	}
}

// Find a closing double-quote.
func stringLiteralEnd(p int) int {
	start := p
	for ; source[p] != '"'; p++ {
		if source[p] == '\n' || source[p] == '\x00' {
			failAt(start, "unclosed string literal")
		}
		if source[p] == '\\' {
			p++
		}
	}
	return p
}

func readStringLiteral(start, quote int) *Token {
	end := stringLiteralEnd(quote + 1)
	buf := make([]byte, end-quote)
	len := 0

	for p := quote + 1; p < end; {
		if source[p] == '\\' {
			buf[len] = byte(readEscapedChar(&p, p+1))
			len++
		} else {
			buf[len] = source[p]
			len++
			p++
		}
	}

	tok := NewToken(TK_STR, start, end+1-start, source[start:end+1])
	tok.ty = arrayOf(tyChar, len+1)
	if !strings.HasSuffix(string(buf), "\x00") {
		tok.str = fmt.Sprintf("%s\x00", buf)
	} else {
		tok.str = string(buf)
	}
	return tok
}

// Read a UTF-8-encoded string literal and transcode it in UTF-16.
//
// UTF-16 is yet another variable-width encoding for Unicode. Code
// points smaller than U+10000 are encoded in 2 bytes. Code points
// equal to or larger than that are encoded in 4 bytes. Each 2 bytes
// in the 4 byte sequence is called "surrogate", and a 4 byte sequence
// is called a "surrogate pair".
func readUtf16StringLiteral(start, quote int) *Token {
	end := stringLiteralEnd(quote + 1)
	buf := make([]uint16, end-start)
	l := 0

	for p := quote + 1; p < end; {
		if source[p] == '\\' {
			buf[l] = uint16(readEscapedChar(&p, p+1))
			l++
			continue
		}

		c := decodeUtf8(&p, p)
		if c < 0x10000 {
			// Encode a code point in 2 bytes.
			buf[l] = uint16(c)
			l++
		} else {
			// Encode a code point in 4 bytes.
			c -= 0x10000
			buf[l] = uint16(0xd800 + ((c >> 10) & 0x3ff))
			l++
			buf[l] = uint16(0xdc00 + (c & 0x3ff))
			l++
		}
	}

	tok := NewToken(TK_STR, start, end+1-start, source[start:end+1])
	tok.ty = arrayOf(tyUShort, l+1)

	bs := make([]byte, len(buf)*2)
	for i, v := range buf {
		binary.LittleEndian.PutUint16(bs[i*2:], v)
	}
	tok.str = string(bs)
	return tok
}

// Read a UTF-8-encoded string literal and transcode it in UTF-32.
//
// UTF-32 is a fixed-width encoding for Unicode. Each code point is
// encoded in 4 bytes.
func readUtf32StringLiteral(start, quote int, ty *Type) *Token {
	end := stringLiteralEnd(quote + 1)
	buf := make([]uint32, end-quote)
	l := 0

	for p := quote + 1; p < end; {
		if source[p] == '\\' {
			buf[l] = uint32(readEscapedChar(&p, p+1))
			l++
		} else {
			buf[l] = decodeUtf8(&p, p)
			l++
		}
	}

	tok := NewToken(TK_STR, start, end+1-start, source[start:end+1])
	tok.ty = arrayOf(ty, l+1)

	bs := make([]byte, len(buf)*4)
	for i, v := range buf {
		binary.LittleEndian.PutUint32(bs[i*4:], v)
	}
	tok.str = string(bs)
	return tok
}

func readCharLiteral(start, quote int, ty *Type) *Token {
	p := quote + 1
	if source[p] == '\x00' {
		failAt(start, "unclosed char literal")
	}

	var c int
	if source[p] == '\\' {
		c = int(int8(readEscapedChar(&p, p+1)))
	} else {
		c = int(decodeUtf8(&p, p))
	}

	end := strings.Index(source[p:], "'")
	if end == -1 {
		failAt(p, "unclosed char literal")
	}

	tok := NewToken(TK_NUM, start, p+end-start+1, source[start:p+end+1])
	tok.val = int64(c)
	tok.ty = ty
	return tok
}

func convertPPInt(tok *Token) bool {
	p := tok.loc

	// Read a binary, octal, decimal or hexadecimal number.
	base := 10
	if strings.HasPrefix(strings.ToLower(source[p:p+2]), "0x") && isXDigit(source[p+2]) {
		p += 2
		base = 16
	} else if strings.HasPrefix(strings.ToLower(source[p:p+2]), "0b") && (source[p+2] == '0' || source[p+2] == '1') {
		p += 2
		base = 2
	} else if source[p] == '0' {
		base = 8
	}

	end := getIntegerEnd(source[p:], base)
	val, _ := strconv.ParseUint(source[p:p+end], base, 64)

	p += end

	// Read U, L or LL suffixes.
	l := false
	u := false

	if strings.HasPrefix(source[p:], "LLU") || strings.HasPrefix(source[p:], "LLu") ||
		strings.HasPrefix(source[p:], "llU") || strings.HasPrefix(source[p:], "llu") ||
		strings.HasPrefix(source[p:], "ULL") || strings.HasPrefix(source[p:], "Ull") ||
		strings.HasPrefix(source[p:], "uLL") || strings.HasPrefix(source[p:], "ull") {
		p += 3
		u = true
		l = true
	} else if strings.HasPrefix(strings.ToLower(source[p:]), "lu") || strings.HasPrefix(strings.ToLower(source[p:]), "ul") {
		p += 2
		u = true
		l = true
	} else if strings.HasPrefix(source[p:], "LL") || strings.HasPrefix(source[p:], "ll") {
		p += 2
		l = true
	} else if source[p] == 'L' || source[p] == 'l' {
		p++
		l = true
	} else if source[p] == 'U' || source[p] == 'u' {
		p++
		u = true
	}

	if p != tok.loc+tok.len {
		return false
	}

	// Infer a type.
	var ty *Type
	if base == 10 {
		if l && u {
			ty = tyULong
		} else if l {
			ty = tyLong
		} else if u {
			if val>>32 != 0 {
				ty = tyULong
			} else {
				ty = tyUInt
			}
		} else {
			if val>>31 != 0 {
				ty = tyLong
			} else {
				ty = tyInt
			}
		}
	} else {
		if l && u {
			ty = tyULong
		} else if l {
			if val>>63 != 0 {
				ty = tyULong
			} else {
				ty = tyLong
			}
		} else if u {
			if val>>32 != 0 {
				ty = tyULong
			} else {
				ty = tyUInt
			}
		} else if val>>63 != 0 {
			ty = tyULong
		} else if val>>32 != 0 {
			ty = tyLong
		} else if val>>31 != 0 {
			ty = tyUInt
		} else {
			ty = tyInt
		}
	}

	tok.kind = TK_NUM
	tok.val = int64(val)
	tok.ty = ty
	return true
}

// The definition of the numeric literal at the preprocessing stage
// is more relaxed than the definition of that at the later stages.
// In order to handle that, a numeric literal is tokenized as a
// "pp-number" token first and then converted to a regular number
// token after preprocessing.
//
// This function converts a pp-number token to a regular number token.
func convertPPNumber(tok *Token) {
	source = tok.file.contents
	// Try to parse as an integer constant.
	if convertPPInt(tok) {
		return
	}

	// If it's not an integer, it must be a floating point constant.
	p := tok.loc
	end := getFloatEnd(source[p:])
	val, err := strconv.ParseFloat(source[p:p+end], 64)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: invalid argument convert to float: %s\n", source[p:p+end], err)
		os.Exit(1)
	}
	p += end

	var ty *Type
	switch source[p] {
	case 'f', 'F':
		ty = tyFloat
		p++
	case 'l', 'L':
		ty = tyDouble
		p++
	default:
		ty = tyDouble
	}

	if tok.loc+tok.len != p {
		failTok(tok, "invalid numeric constant")
	}
	tok.kind = TK_NUM
	tok.fval = val
	tok.ty = ty
}

func convertPPTokens(tok *Token) {
	for t := tok; t.kind != TK_EOF; t = t.next {
		if isKeyword(t) {
			t.kind = TK_KEYWORD
		} else if t.kind == TK_PP_NUM {
			convertPPNumber(t)
		}
	}
}

// Initialize line info for all tokens.
func addLineNumbers(tok *Token) {
	p := 0
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
		if p >= len(source)-1 {
			break
		}
	}
}

func tokenizeStringLiteral(tok *Token, basety *Type) *Token {
	source = tok.file.contents
	var t *Token
	if basety.size == 2 {
		t = readUtf16StringLiteral(tok.loc, tok.loc)
	} else {
		t = readUtf32StringLiteral(tok.loc, tok.loc, basety)
	}
	t.next = tok.next
	return t
}

// Tokenize a given string and returns new tokens.
func tokenize(file *File) *Token {
	currentFile = file
	source = currentFile.contents

	input := file.contents
	head := Token{}
	cur := &head
	p := 0

	atBol = true
	hasSpace = false

	for p < len(input) {
		// Skip line comments.
		if strings.HasPrefix(input[p:], "//") {
			p += 2
			for input[p] != '\n' {
				p++
			}
			hasSpace = true
			continue
		}

		// Skip block comments.
		if strings.HasPrefix(input[p:], "/*") {
			q := strings.Index(input[p+2:], "*/")
			if q == -1 {
				failAt(p, "unclosed block comment")
			}
			p = p + 2 + q + 2
			hasSpace = true
			continue
		}

		// Skip newline.
		if input[p] == '\n' {
			p++
			atBol = true
			hasSpace = false
			continue
		}

		// Skip whitespace characters.
		if unicode.IsSpace(rune(input[p])) {
			p++
			hasSpace = true
			continue
		}

		// Numeric literal
		if unicode.IsDigit(rune(input[p])) || (input[p] == '.' && unicode.IsDigit(rune(input[p+1]))) {
			q := p
			p++
			for {
				if input[p] != '\x00' && input[p+1] != '\x00' &&
					strings.Contains("eEpP", string(source[p])) &&
					strings.Contains("+-", string(source[p+1])) {
					p += 2
				} else if unicode.IsDigit(rune(source[p])) || unicode.IsLetter(rune(source[p])) || input[p] == '.' {
					p++
				} else {
					break
				}
			}
			cur.next = NewToken(TK_PP_NUM, q, p-q, source[q:p])
			cur = cur.next
			continue
		}

		// String literal
		if input[p] == '"' {
			cur.next = readStringLiteral(p, p)
			cur = cur.next
			p += cur.len
			continue
		}

		// UTF-8 string literal
		if strings.HasPrefix(input[p:], "u8\"") {
			cur.next = readStringLiteral(p, p+2)
			cur = cur.next
			p += cur.len
			continue
		}

		// UTF-16 string literal
		if strings.HasPrefix(input[p:], "u\"") {
			cur.next = readUtf16StringLiteral(p, p+1)
			cur = cur.next
			p += cur.len
			continue
		}

		// Wide string literal
		if strings.HasPrefix(input[p:], "L\"") {
			cur.next = readUtf32StringLiteral(p, p+1, tyInt)
			cur = cur.next
			p += cur.len
			continue
		}

		// UTF-32 string literal
		if strings.HasPrefix(input[p:], "U\"") {
			cur.next = readUtf32StringLiteral(p, p+1, tyUInt)
			cur = cur.next
			p += cur.len
			continue
		}

		// Character literal
		if input[p] == '\'' {
			cur.next = readCharLiteral(p, p, tyInt)
			cur.val = int64(int8(cur.val))
			cur = cur.next
			p += cur.len
			continue
		}

		// UTF-16 character literal
		if strings.HasPrefix(input[p:], "u'") {
			cur.next = readCharLiteral(p, p+1, tyUShort)
			cur = cur.next
			cur.val &= 0xffff
			p += cur.len
			continue
		}

		// Wide character literal
		if strings.HasPrefix(input[p:], "L'") {
			cur.next = readCharLiteral(p, p+1, tyInt)
			cur = cur.next
			p += cur.len
			continue
		}

		// UTF-32 character literal
		if strings.HasPrefix(input[p:], "U'") {
			cur.next = readCharLiteral(p, p+1, tyUInt)
			cur = cur.next
			p += cur.len
			continue
		}

		// Identifier or keyword
		identLen := readIdent(p)
		if identLen != 0 {
			cur.next = NewToken(TK_IDENT, p, identLen, source[p:p+identLen])
			cur = cur.next
			p += cur.len
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

		if input[p] == '\x00' {
			p++
			continue
		}

		failAt(p, "invalid token")
	}

	cur.next = NewToken(TK_EOF, p, 0, "")
	addLineNumbers(head.next)
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
		return ""
	}

	content := string(data)
	if len(content) > 0 && content[len(content)-1] != '\n' {
		content += "\n"
	}
	return content
}

// Replaces \r or \r\n with \n.
func canonicalizeNewline(p []byte) []byte {
	i, j := 0, 0
	for p[i] != '\x00' {
		if p[i] == '\r' && p[i+1] == '\n' {
			i += 2
			p[j] = '\n'
			j++
		} else if p[i] == '\r' {
			i++
			p[j] = '\n'
			j++
		} else {
			p[j] = p[i]
			j++
			i++
		}
	}

	p[j] = '\x00'
	return p[:j+1]
}

// Removes backslashes followed by a newline.
func removeBackSlashNewline(p []byte) []byte {
	i, j := 0, 0

	// We want to keep the number of newline characters so that
	// the logical line number matches the physical one.
	// This counter maintain the number of newlines we have removed.
	n := 0

	for p[i] != '\x00' {
		if p[i] == '\\' && p[i+1] == '\n' {
			i += 2
			n++
		} else if p[i] == '\n' {
			p[j] = p[i]
			j++
			i++
			for ; n > 0; n-- {
				p[j] = '\n'
				j++
			}
		} else {
			p[j] = p[i]
			j++
			i++
		}
	}

	for ; n > 0; n-- {
		p[j] = '\n'
		j++
	}

	p[j] = '\x00'

	return p[:j+1]
}

func readUniversalChar(p []byte, len int) uint32 {
	c := uint32(0)
	for i := range len {
		if !isXDigit(p[i]) {
			return 0
		}
		c = (c << 4) | uint32(fromHex((p[i])))
	}
	return c
}

// Replace \u or \U escape sequences with corresponding UTF-8 bytes.
func convertUniversalChars(p []byte) []byte {
	q := p
	pi, qi := 0, 0

	for p[pi] != '\x00' {
		if strings.HasPrefix(string(p[pi:]), "\\u") {
			c := readUniversalChar(p[pi+2:], 4)
			if c != 0 {
				pi += 6
				qi += encodeUtf8(q[qi:], c)
			} else {
				q[qi] = p[pi]
				qi++
				pi++
			}
		} else if strings.HasPrefix(string(p[pi:]), "\\U") {
			c := readUniversalChar(p[pi+2:], 8)
			if c != 0 {
				pi += 10
				qi += encodeUtf8(q[qi:], c)
			} else {
				q[qi] = p[pi]
				qi++
				pi++
			}
		} else if p[pi] == '\\' {
			q[qi] = p[pi]
			qi++
			pi++
			q[qi] = p[pi]
			qi++
			pi++
		} else {
			q[qi] = p[pi]
			qi++
			pi++
		}
	}

	q[qi] = '\x00'
	return q[:qi+1]
}

var fileno int

func tokenizeFile(path string) *Token {
	p := readFile(path)

	if len(p) == 0 {
		return nil
	}

	cs := canonicalizeNewline([]byte(p + "\x00"))
	cs = removeBackSlashNewline(cs)
	cs = convertUniversalChars(cs)

	// Save the filename for assembler .file directive.
	file := newFile(path, fileno+1, string(cs))
	inputfiles = append(inputfiles, make([]*File, fileno+2-len(inputfiles))...)
	inputfiles[fileno] = file
	inputfiles[fileno+1] = nil

	fileno++

	return tokenize(file)
}

func getIntegerEnd(s string, base int) int {
	end := 0
	switch base {
	case 16:
		for isXDigit(s[end]) {
			end++
		}
	case 10:
		for s[end] >= '0' && s[end] <= '9' {
			end++
		}
	case 8:
		for s[end] >= '0' && s[end] <= '7' {
			end++
		}
	case 2:
		for s[end] == '0' || s[end] == '1' {
			end++
		}
	}
	return end
}

func getFloatEnd(s string) int {
	if s[0] == '0' && (s[1] == 'x' || s[1] == 'X') {
		return getHexFloatEnd(s)
	}
	return getDecimalFloatEnd(s)
}

func getHexFloatEnd(s string) int {
	i := 0
	if s[i] != '0' || (s[i+1] != 'x' && s[i+1] != 'X') {
		return -1
	}
	i += 2

	for isXDigit(s[i]) || s[i] == '.' {
		i++
	}

	if s[i] != 'p' && s[i] != 'P' {
		return i
	}
	i++

	expStart := i
	for unicode.IsDigit(rune(s[i])) || s[i] == '+' || s[i] == '-' {
		i++
	}

	if i == expStart {
		return expStart - 1
	}

	return i
}

func getDecimalFloatEnd(s string) int {
	i := 0
	for unicode.IsDigit(rune(s[i])) {
		i++
	}

	if s[i] == '.' {
		i++
		for unicode.IsDigit(rune(s[i])) {
			i++
		}
	}

	if s[i] == 'e' || s[i] == 'E' {
		i++
		if s[i] == '+' || s[i] == '-' {
			i++
		}
		for unicode.IsDigit(rune(s[i])) {
			i++
		}
	}

	return i
}

func isXDigit(c byte) bool {
	return (c >= '0' && c <= '9') ||
		(c >= 'a' && c <= 'f') ||
		(c >= 'A' && c <= 'F')
}
