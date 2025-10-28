package main

import (
	"fmt"
	"os"
	"strconv"
	"unicode"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "%s: invalid number of arguments\n", os.Args[0])
		os.Exit(1)
	}

	input := os.Args[1]

	fmt.Printf("  .globl main\n")
	fmt.Printf("main:\n")
	n, p := parseNumber(input, 0)
	fmt.Printf("  li a0, %d\n", n)

	for p < len(input) {
		switch input[p] {
		case '+':
			p++
			n, p = parseNumber(input, p)
			fmt.Printf("  addi a0, a0, %d\n", n)
		case '-':
			p++
			n, p = parseNumber(input, p)
			fmt.Printf("  addi a0, a0, -%d\n", n)
		default:
			fmt.Fprintf(os.Stderr, "unexpected character: '%c'\n", input[p])
			os.Exit(1)
		}
	}

	fmt.Printf("  ret\n")
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
