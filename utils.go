package main

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

func unreachable() {
	_, file, line, ok := runtime.Caller(1)
	if !ok {
		panic("failed to get caller info")
	}

	fileContent, err := os.ReadFile(file)
	if err != nil {
		panic(fmt.Sprintf("failed to read file: %v", err))
	}

	lines := strings.Split(string(fileContent), "\n")
	if line-1 >= len(lines) {
		panic("invalid line number")
	}

	fail("internal error at %s:%d", filepath.Base(file), line)
}

// Reports an error and exit.
func fail(format string, args ...any) {
	fmt.Fprintf(os.Stderr, format+"\n", args...)
	os.Exit(1)
}

// Reports an error message in the following format.
// foo.c:10: x = y + 1;
// ^ <error message here>
func vfailAt(filename, input string, lineno, loc int, format string, args ...any) {
	// Find a line containing `loc`.
	line := loc
	for 0 < line && input[line-1] != '\n' {
		line--
	}

	end := loc
	for input[end] != '\x00' && input[end] != '\n' {
		end++
	}

	// Print out the line.
	indent, _ := fmt.Fprintf(os.Stderr, "%s:%d: ", filename, lineno)
	fmt.Fprintf(os.Stderr, "%.*s\n", end-line, input[line:])

	// Show the error message.
	pos := loc - line + indent

	fmt.Fprintf(os.Stderr, "%*s", pos, "") // print pos spaces
	fmt.Fprint(os.Stderr, "^ ")
	fmt.Fprintf(os.Stderr, format+"\n", args...)
}

func failAt(loc int, format string, args ...any) {
	// Get a line number.
	lineno := 1
	for p := 0; p < loc; p++ {
		if source[p] == '\n' {
			lineno++
		}
	}
	vfailAt(currentFile.name, currentFile.contents, lineno, loc, format, args...)
	os.Exit(1)
}

func failTok(tok *Token, format string, args ...any) {
	vfailAt(tok.file.name, tok.file.contents, tok.lineno, tok.loc, format, args...)
	os.Exit(1)
}

func warnTok(tok *Token, format string, args ...any) {
	vfailAt(tok.file.name, tok.file.contents, tok.lineno, tok.loc, format, args...)
}

func assert(condition bool) {
	if condition {
		return
	}

	pc, file, line, ok := runtime.Caller(1)
	if !ok {
		panic("failed to get caller info")
	}

	frames := runtime.CallersFrames([]uintptr{pc})
	frame, _ := frames.Next()
	funcName := frame.Function

	fileContent, err := os.ReadFile(file)
	if err != nil {
		panic(fmt.Sprintf("failed to read file: %v", err))
	}

	lines := strings.Split(string(fileContent), "\n")
	if line-1 >= len(lines) {
		panic("invalid line number")
	}
	expr := strings.TrimSpace(lines[line-1])
	start := strings.Index(expr, "(")
	end := strings.LastIndex(expr, ")")
	if start != -1 && end != -1 && end > start {
		expr = expr[start+1 : end]
	}

	msg := fmt.Sprintf("%s:%d: %s: Assertion `%s` failed\n",
		filepath.Base(file), line, funcName, expr)

	fmt.Fprint(os.Stderr, msg)
	os.Exit(1)
}
