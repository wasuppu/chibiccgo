package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

type FileType int

const (
	FILE_NONE FileType = iota
	FILE_C
	FILE_ASM
	FILE_OBJ
)

var optX FileType
var optFcommon bool = true
var optE bool
var optS bool
var optC bool
var optCC1 bool
var optHashHashHash bool
var optO string
var optMarch string

var basefile string
var outfile string

var ldExtraArgs []string
var optInclude []string
var inputPaths []string
var tmpfiles []string
var includePaths []string

var rvpath = "/opt/riscv-linux"

func usage(status int) {
	fmt.Fprintf(os.Stderr, "chibicc [ -o <path> ] <file>\n")
	os.Exit(status)
}

func takeArg(arg string) bool {
	x := []string{"-o", "-I", "-idirafter", "-include", "-x"}

	for i := range x {
		if arg == x[i] {
			return true
		}
	}

	return false
}

func (a X64) addDefaultIncludePaths(argv0 string) {
	// We expect that chibicc-specific include files are installed
	// to ./include relative to argv[0].
	includePaths = append(includePaths, filepath.Join(filepath.Dir(argv0), "include"))

	// Add standard include paths.
	includePaths = append(includePaths, "/usr/local/include")
	includePaths = append(includePaths, "/usr/include/x86_64-linux-gnu")
	includePaths = append(includePaths, "/usr/include")
}

func (a RiscV) addDefaultIncludePaths(argv0 string) {
	// We expect that chibicc-specific include files are installed
	// to ./include relative to argv[0].
	includePaths = append(includePaths, filepath.Join(filepath.Dir(argv0), "include"))

	// Add standard include paths.
	includePaths = append(includePaths, "/usr/local/include")
	includePaths = append(includePaths, "/usr/include/riscv64-linux-gnu")
	includePaths = append(includePaths, filepath.Join(rvpath, "riscv64-unknown-linux-gnu/include"))
	includePaths = append(includePaths, "/usr/include")
	includePaths = append(includePaths, filepath.Join(rvpath, "sysroot/usr/include"))
}

func define(str string) {
	eq := strings.Index(str, "=")

	if eq != -1 {
		defineMacro(str[:eq], str[eq+1:])
	} else {
		defineMacro(str, "1")
	}
}

func parseOptX(s string) FileType {
	if s == "c" {
		return FILE_C
	}
	if s == "assembler" {
		return FILE_ASM
	}
	if s == "none" {
		return FILE_NONE
	}
	fail(fmt.Sprintf("<command line>: unknown argument for -x: %s", s))
	return -1
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

	var idirafter []string

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

		if args[i] == "-fcommon" {
			optFcommon = true
			continue
		}

		if args[i] == "-fno-common" {
			optFcommon = false
			continue
		}

		if args[i] == "-c" {
			optC = true
			continue
		}

		if args[i] == "-E" {
			optE = true
			continue
		}

		if strings.HasPrefix(args[i], "-I") {
			includePaths = append(includePaths, args[i][2:])
			continue
		}

		if args[i] == "-D" {
			i++
			define(args[i])
			continue
		}

		if strings.HasPrefix(args[i], "-D") {
			define(args[i][2:])
			continue
		}

		if args[i] == "-U" {
			i++
			undefMacro(args[i])
			continue
		}

		if strings.HasPrefix(args[i], "-U") {
			undefMacro(args[i][2:])
			continue
		}

		if args[i] == "-include" {
			i++
			optInclude = append(optInclude, args[i])
			continue
		}

		if args[i] == "-x" {
			i++
			optX = parseOptX(args[i])
			continue
		}

		if strings.HasPrefix(args[i], "-x") {
			optX = parseOptX(args[i][2:])
			continue
		}

		if strings.HasPrefix(args[i], "-l") {
			inputPaths = append(inputPaths, args[i])
			continue
		}

		if args[i] == "-s" {
			ldExtraArgs = append(ldExtraArgs, "-s")
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

		if args[i] == "-idirafter" {
			idirafter = append(idirafter, args[i])
			i++
			continue
		}

		// These options are ignored for now.
		if strings.HasPrefix(args[i], "-O") ||
			strings.HasPrefix(args[i], "-W") ||
			strings.HasPrefix(args[i], "-g") ||
			strings.HasPrefix(args[i], "-std=") ||
			args[i] == "-ffreestanding" ||
			args[i] == "-fno-builtin" ||
			args[i] == "-fno-omit-frame-pointer" ||
			args[i] == "-fno-stack-protector" ||
			args[i] == "-fno-strict-aliasing" ||
			args[i] == "-m64" ||
			args[i] == "-mno-red-zone" ||
			args[i] == "-w" {
			continue
		}

		if args[i][0] == '-' && len(args[i]) > 1 {
			fail("unknown argument: %s", args[i])
		}

		inputPaths = append(inputPaths, args[i])
	}

	includePaths = append(includePaths, idirafter...)

	if len(inputPaths) == 0 {
		fail("no input files")
	}

	// -E implies that the input is the C macro language.
	if optE {
		optX = FILE_C
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

// Print tokens to stdout. Used for -E.
func printTokens(tok *Token) {
	var out *os.File
	if len(optO) > 0 {
		out = openFile(optO)
	} else {
		out = openFile("-")
	}

	line := 1
	for ; tok.kind != TK_EOF; tok = tok.next {
		if line > 1 && tok.atBol {
			fmt.Fprintf(out, "\n")
		}
		if tok.hasSpace && !tok.atBol {
			fmt.Fprintf(out, " ")
		}
		fmt.Fprintf(out, "%s", tok.lexeme)
		line++
	}
	fmt.Fprintf(out, "\n")
}

func mustTokenizeFile(path string) *Token {
	tok := tokenizeFile(path)
	if tok == nil {
		fail("fail to tokenize %s", path)
	}
	return tok
}

func appendTokens(tok1, tok2 *Token) *Token {
	if tok1 == nil || tok1.kind == TK_EOF {
		return tok2
	}

	t := tok1
	for t.next.kind != TK_EOF {
		t = t.next
	}
	t.next = tok2
	return tok1
}

func cc1(target Arch) {
	var tok *Token = nil

	// Process -include option
	for i := 0; i < len(optInclude); i++ {
		incl := optInclude[i]

		var path string
		if fileExists(incl) {
			path = incl
		} else {
			path = searchIncludePaths(incl)
			if len(path) == 0 {
				fail("-include: %s", incl)
			}
		}

		tok2 := mustTokenizeFile(path)
		tok = appendTokens(tok, tok2)
	}

	// Tokenize and parse.
	tok2 := mustTokenizeFile(basefile)

	tok = appendTokens(tok, tok2)
	tok = preprocess(tok)

	// If -E is given, print out preprocessed C code as a result.
	if optE {
		printTokens(tok)
		return
	}

	prog := parse(tok)

	// Open a temporary output buffer.
	var outputBuf bytes.Buffer

	// Traverse the AST to emit assembly.
	codegen(target, prog, &outputBuf)

	// Write the asembly text to a file.
	out := openFile(outfile)
	defer out.Close()
	if _, err := io.Copy(out, &outputBuf); err != nil {
		fail(fmt.Sprintf("fail to write buf to file %s:%s", outfile, err))
	}
}

func (a X64) assemble(input, output string) {
	cmd := []string{"as", "-c", input, "-o", output}
	runSubprocess(cmd)
}

func (a RiscV) assemble(input, output string) {
	cmd := []string{"riscv64-unknown-linux-gnu-as", "-c", input, "-o", output}
	runSubprocess(cmd)
}

func findFile(pattern string) string {
	matches, err := filepath.Glob(pattern)
	if err != nil {
		fail(fmt.Sprintf("fail to exec file pattern %s:%s", pattern, err))
	}
	if len(matches) == 0 {
		fail("no matching files found")
	}
	return matches[len(matches)-1]
}

// Returns true if a given file exists.
func fileExists(path string) bool {
	_, err := os.Stat(path)
	return !os.IsNotExist(err)
}

func (a X64) findLibPath() string {
	paths := []string{
		"/usr/lib/x86_64-linux-gnu/crti.o",
		"/usr/lib64/crti.o",
	}

	for _, path := range paths {
		if fileExists(path) {
			return filepath.Dir(path)
		}
	}

	fail("library path is not found")
	return ""
}

func (a RiscV) findLibPath() string {
	paths := []string{
		"/usr/lib/riscv64-linux-gnu/crti.o",
		"/usr/lib64/crti.o",
		filepath.Join(rvpath, "sysroot/usr/lib/crti.o"),
	}

	for _, path := range paths {
		if fileExists(path) {
			return filepath.Dir(path)
		}
	}

	fail("library path is not found")
	return ""
}

func (a X64) findGCCLibPath() string {
	paths := []string{
		"/usr/lib/gcc/x86_64-linux-gnu/*/crtbegin.o",
		"/usr/lib/gcc/x86_64-pc-linux-gnu/*/crtbegin.o", // For Gentoo
		"/usr/lib/gcc/x86_64-redhat-linux/*/crtbegin.o", // For Fedora
	}

	for i := range paths {
		path := findFile(paths[i])
		if len(path) > 0 {
			return filepath.Dir(path)
		}
	}

	fail("gcc library path is not found")
	return ""
}

func (a RiscV) findGCCLibPath() string {
	paths := []string{
		"/usr/lib/gcc/riscv64-linux-gnu/*/crtbegin.o",
		"/usr/lib/gcc/riscv64-pc-linux-gnu/*/crtbegin.o", // For Gentoo
		"/usr/lib/gcc/riscv64-redhat-linux/*/crtbegin.o", // For Fedora
		filepath.Join(rvpath, "lib/gcc/riscv64-unknown-linux-gnu/*/crtbegin.o"),
	}

	for i := range paths {
		path := findFile(paths[i])
		if len(path) > 0 {
			return filepath.Dir(path)
		}
	}

	fail("gcc library path is not found")
	return ""
}

func (a X64) runLinker(inputs []string, output string) {
	var arr []string

	arr = append(arr, "ld")
	arr = append(arr, "-o")
	arr = append(arr, output)
	arr = append(arr, "-m")
	arr = append(arr, "elf_x86_64")
	arr = append(arr, "-dynamic-linker")
	arr = append(arr, "/lib64/ld-linux-x86-64.so.2")

	libpath := a.findLibPath()
	gccLibpath := a.findGCCLibPath()

	arr = append(arr, fmt.Sprintf("%s/crt1.o", libpath))
	arr = append(arr, fmt.Sprintf("%s/crti.o", libpath))
	arr = append(arr, fmt.Sprintf("%s/crtbegin.o", gccLibpath))
	arr = append(arr, fmt.Sprintf("-L%s", gccLibpath))
	arr = append(arr, fmt.Sprintf("-L%s", libpath))
	arr = append(arr, fmt.Sprintf("-L%s/..", libpath))
	arr = append(arr, "-L/usr/lib64")
	arr = append(arr, "-L/lib64")
	arr = append(arr, "-L/usr/lib/x86_64-linux-gnu")
	arr = append(arr, "-L/usr/lib/x86_64-pc-linux-gnu")
	arr = append(arr, "-L/usr/lib/x86_64-redhat-linux")
	arr = append(arr, "-L/usr/lib")
	arr = append(arr, "-L/lib")

	for i := 0; i < len(ldExtraArgs); i++ {
		arr = append(arr, ldExtraArgs[i])
	}

	arr = append(arr, inputs...)

	arr = append(arr, "-lc")
	arr = append(arr, "-lgcc")
	arr = append(arr, "--as-needed")
	arr = append(arr, "-lgcc_s")
	arr = append(arr, "--no-as-needed")
	arr = append(arr, fmt.Sprintf("%s/crtend.o", gccLibpath))
	arr = append(arr, fmt.Sprintf("%s/crtn.o", libpath))

	runSubprocess(arr)
}

func (a RiscV) runLinker(inputs []string, output string) {
	var arr []string

	arr = append(arr, "riscv64-unknown-linux-gnu-ld")
	arr = append(arr, "-o")
	arr = append(arr, output)
	arr = append(arr, "-m")
	arr = append(arr, "elf64lriscv")
	arr = append(arr, "-dynamic-linker")
	arr = append(arr, filepath.Join(rvpath, "sysroot/lib/ld-linux-riscv64-lp64d.so.1"))

	libpath := a.findLibPath()
	gccLibpath := a.findGCCLibPath()

	arr = append(arr, fmt.Sprintf("%s/crt1.o", libpath))
	arr = append(arr, fmt.Sprintf("%s/crti.o", libpath))
	arr = append(arr, fmt.Sprintf("%s/crtbegin.o", gccLibpath))
	arr = append(arr, fmt.Sprintf("-L%s", gccLibpath))
	arr = append(arr, fmt.Sprintf("-L%s", libpath))
	arr = append(arr, fmt.Sprintf("-L%s/..", libpath))

	arr = append(arr, fmt.Sprintf("-L%s/sysroot/usr/lib64", rvpath))
	arr = append(arr, fmt.Sprintf("-L%s/sysroot/lib64", rvpath))
	arr = append(arr, fmt.Sprintf("-L%s/sysroot/usr/lib/riscv64-linux-gnu", rvpath))
	arr = append(arr, fmt.Sprintf("-L%s/sysroot/usr/lib/riscv64-pc-linux-gnu", rvpath))
	arr = append(arr, fmt.Sprintf("-L%s/sysroot/usr/lib/riscv64-redhat-linux", rvpath))
	arr = append(arr, fmt.Sprintf("-L%s/sysroot/usr/lib", rvpath))
	arr = append(arr, fmt.Sprintf("-L%s/sysroot//lib", rvpath))

	arr = append(arr, inputs...)

	arr = append(arr, "-lc")
	arr = append(arr, "-lgcc")
	arr = append(arr, "--as-needed")
	arr = append(arr, "-lgcc_s")
	arr = append(arr, "--no-as-needed")
	arr = append(arr, fmt.Sprintf("%s/crtend.o", gccLibpath))
	arr = append(arr, fmt.Sprintf("%s/crtn.o", libpath))

	runSubprocess(arr)
}

func getFileType(filename string) FileType {
	ext := filepath.Ext(filename)
	if ext == ".o" {
		return FILE_OBJ
	}

	if optX != FILE_NONE {
		return optX
	}

	if ext == ".c" {
		return FILE_C
	}

	if ext == ".s" {
		return FILE_ASM
	}

	fail(fmt.Sprintf("<command line>: unknown file extension: %s", filename))
	return -1
}

func main() {
	defer cleanup()

	parseArgs(os.Args)
	target := chooseArch(optMarch)
	target.initMacro()

	if optCC1 {
		target.addDefaultIncludePaths(os.Args[0])
		cc1(target)
		return
	}

	if len(inputPaths) > 1 && len(optO) > 0 && (optC || optS || optE) {
		fail("cannot specify '-o' with '-c,' '-S' or '-E' with multiple files")
	}

	var ldArgs []string

	for i := 0; i < len(inputPaths); i++ {
		input := inputPaths[i]

		if strings.HasPrefix(input, "-l") {
			ldArgs = append(ldArgs, input)
			continue
		}

		var output string
		if len(optO) > 0 {
			output = optO
		} else if optS {
			output = replaceExtn(input, ".s")
		} else {
			output = replaceExtn(input, ".o")
		}

		filetype := getFileType(input)

		// Handle .o
		if filetype == FILE_OBJ {
			ldArgs = append(ldArgs, input)
			continue
		}

		// Handle .s
		if filetype == FILE_ASM {
			if !optS {
				target.assemble(input, output)
			}
			continue
		}

		assert(filetype == FILE_C)

		// Just preprocess
		if optE {
			runCC1(os.Args, input, "")
			continue
		}

		// compile
		if optS {
			runCC1(os.Args, input, output)
			continue
		}

		// Compile and assemble
		if optC {
			tmp := createTmpfile()
			runCC1(os.Args, input, tmp)
			target.assemble(tmp, output)
			continue
		}

		// Compile, assemble and link
		tmp1 := createTmpfile()
		tmp2 := createTmpfile()
		runCC1(os.Args, input, tmp1)
		target.assemble(tmp1, tmp2)
		ldArgs = append(ldArgs, tmp2)
		continue
	}

	if len(ldArgs) > 0 {
		if len(optO) > 0 {
			target.runLinker(ldArgs, optO)
		} else {
			target.runLinker(ldArgs, "a.out")
		}
	}
}
