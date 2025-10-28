package main

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
)

var optCC1 bool
var optHashHashHash bool
var optO string
var optMarch string

var inputPath string

func usage(status int) {
	fmt.Fprintf(os.Stderr, "chibicc [ -o <path> ] <file>\n")
	os.Exit(status)
}

func parseArgs(args []string) {
	for i := 1; i < len(args); i++ {
		if args[i] == "-###" {
			optHashHashHash = true
			continue
		}

		if args[i] == "-cc1" {
			optCC1 = true
			continue
		}

		if args[i] == "--help" {
			usage(0)
		}

		if args[i] == "-o" {
			i++
			if len(args[i]) == 0 {
				usage(1)
			}
			optO = args[i]
			continue
		}

		if strings.HasPrefix(args[i], "-o") {
			optO = args[i][2:]
			continue
		}

		if strings.HasPrefix(args[i], "-march=") {
			optMarch = args[i][7:]
			continue
		}

		if args[i][0] == '-' && len(args[i]) > 1 {
			fail("unknown argument: %s", args[i])
		}

		inputPath = args[i]
	}

	if len(inputPath) == 0 {
		fail("no input files")
	}

	if len(optMarch) == 0 {
		optMarch = "x64"
	}
}

func openFile(path string) *os.File {
	if len(path) == 0 || path == "-" {
		return os.Stdout
	}

	out, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0600)
	if err != nil {
		fail("cannot open output file: %s: %s", path, err)
	}
	return out
}

func runSubprocess(args []string) {
	// If -### is given, dump the subprocess's command line.
	if optHashHashHash {
		fmt.Fprint(os.Stderr, args[0])
		for i := 1; i < len(args); i++ {
			fmt.Fprintf(os.Stderr, " %s", args[i])
		}
		fmt.Fprint(os.Stderr, "\n")
	}

	cmd := exec.Command(args[0], args[1:]...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	// Child process. Run a new command.
	if err := cmd.Run(); err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			os.Exit(exitErr.ExitCode())
		}
		fmt.Fprintf(os.Stderr, "exec failed: %s: %v", args[0], err)
		os.Exit(1)
	}
}

func runCC1(args []string) {
	runSubprocess(append(args, "-cc1"))
}

func cc1() {
	target := chooseArch(optMarch)

	// Tokenize and parse.
	tok := tokenizeFile(inputPath)
	prog := parse(tok)

	// Traverse the AST to emit assembly.
	out := openFile(optO)
	fmt.Fprintf(out, ".file 1 \"%s\"\n", inputPath)
	codegen(target, prog, out)
}

func main() {
	parseArgs(os.Args)

	if optCC1 {
		cc1()
		return
	}

	runCC1(os.Args)
}
