package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

var optS bool
var optCC1 bool
var optHashHashHash bool
var optO string
var optMarch string

var basefile string
var outfile string

var inputPaths []string
var tmpfiles []string

func usage(status int) {
	fmt.Fprintf(os.Stderr, "chibicc [ -o <path> ] <file>\n")
	os.Exit(status)
}

func takeArg(arg string) bool {
	return arg == "-o"
}

func parseArgs(args []string) {
	// Make sure that all command line options that take an argument
	// have an argument.
	for i := 1; i < len(args); i++ {
		if takeArg(args[i]) {
			i++
			if len(args[i]) == 0 {
				usage(1)
			}
		}
	}

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

		if args[i] == "-S" {
			optS = true
			continue
		}

		if args[i] == "-cc1-input" {
			i++
			basefile = args[i]
			continue
		}

		if args[i] == "-cc1-output" {
			i++
			outfile = args[i]
			continue
		}

		if args[i][0] == '-' && len(args[i]) > 1 {
			fail("unknown argument: %s", args[i])
		}

		inputPaths = append(inputPaths, args[i])
	}

	if len(inputPaths) == 0 {
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

// Replace file extension
func replaceExtn(tmpl string, extn string) string {
	filename := filepath.Base(tmpl)
	dot := strings.LastIndexByte(filename, '.')
	if dot != -1 {
		filename = filename[:dot]
	}
	return filename + extn
}

func cleanup() {
	for i := range tmpfiles {
		os.Remove(tmpfiles[i])
	}
}

func createTmpfile() string {
	file, err := os.CreateTemp("", "chibicc-")
	if err != nil {
		panic("mkstemp failed: " + err.Error())
	}
	defer file.Close()

	path := file.Name()
	tmpfiles = append(tmpfiles, path)
	return path
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

func runCC1(args []string, input, output string) {
	args = append(args, "-cc1")

	if len(input) > 0 {
		args = append(args, "-cc1-input")
		args = append(args, input)
	}

	if len(output) > 0 {
		args = append(args, "-cc1-output")
		args = append(args, output)
	}

	runSubprocess(args)
}

func cc1(target Arch) {
	// Tokenize and parse.
	tok := tokenizeFile(basefile)
	prog := parse(tok)

	// Traverse the AST to emit assembly.
	out := openFile(outfile)
	fmt.Fprintf(out, ".file 1 \"%s\"\n", basefile)
	codegen(target, prog, out)
}

func assemble(input, output string) {
	var cmd []string
	switch ArchName {
	case "x64":
		cmd = []string{"as", "-c", input, "-o", output}
	case "riscv":
		cmd = []string{"riscv64-unknown-linux-gnu-as", "-c", input, "-o", output}
	default:
		fail("invalid arch:", ArchName)
	}
	runSubprocess(cmd)
}

func main() {
	defer cleanup()

	parseArgs(os.Args)
	target := chooseArch(optMarch)

	if optCC1 {
		cc1(target)
		return
	}

	if len(inputPaths) > 1 && len(optO) > 0 {
		fail("cannot specify '-o' with multiple files")
	}

	for i := 0; i < len(inputPaths); i++ {
		input := inputPaths[i]

		var output string
		if len(optO) > 0 {
			output = optO
		} else if optS {
			output = replaceExtn(input, ".s")
		} else {
			output = replaceExtn(input, ".o")
		}

		// If -S is given, assembly text is the final output.
		if optS {
			runCC1(os.Args, input, output)
			continue
		}

		// Otherwise, run the assembler to assemble our output.
		tmpfile := createTmpfile()
		runCC1(os.Args, input, tmpfile)
		assemble(tmpfile, output)
	}
}
