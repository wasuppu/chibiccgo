# chibiccgo

A small C compiler in Go. This is a Go rewrite of Rui Ueyama's [chibicc](https://github.com/rui314/chibicc)‌ compiler, a direct Go port preserving the original code structure and logic. The implementation follows chibicc development trajectory across 316 commits, with selective omissions or merges of the following commits:

- [018] Add LICENSE and README.md
- [035] Refactoring: Add a utility function
- [262] Use __attribute__((format(print, ...))) to find programming errors
- [315] Update README

Initially, this project also referenced [rvcc](https://github.com/sunshaoce/rvcc)'s code to support both x64 and RISC-V architectures simultaneously.
However, RISC-V compatibility began breaking at ‌commit [164] Skip nested #if in a skipped #if-clause. I completely abandoned updates to the RISC-V 
architecture code after [203] Allow to define a function returning a struct.
Subsequently, development proceeded exclusively by referencing the original chibicc code for x64 architecture implementation. The [final commit](https://github.com/wasuppu/chibiccgo/commit/4469c83f2b967515160e7bd7afd381f2d4baadbe) removes all RISC-V code, leaving a pure x64 codebase.

At ‌commit [150] Add stage2 build, self-compilation tests were implemented using Go to compile the original C-based chibicc.
(Testing may require aligning the C chibicc version to specific commits.)

Regarding third-party library compilation, among the five libraries tested by the author, only libpng could be successfully compiled. 
During the cpython compilation process, the following error occurred:

> 1e10000: invalid argument convert to float: strconv.ParseFloat: parsing "1e10000": value out of range

This suggests that the Go implementation might require big.Float type handling? Additionally, sqlite compilation stalled for an excessively long time, 
while other libraries exhibit without clear diagnostic messages.

The stage2 build of chibicc passed sqlite tests in addition to libpng support, with sqlite ultimately showing:

> 0 errors out of 249474 tests on Linux 64-bit little-endian

The three other libraries still failed to pass.

For an overview of chibicc's features, here is a direct quote from the project's original README:


> ## Status
> 
> chibicc supports almost all mandatory features and most optional
> features of C11 as well as a few GCC language extensions.
> 
> Features that are often missing in a small compiler but supported by
> chibicc include (but not limited to):
> 
> - Preprocessor
> - float, double and long double (x87 80-bit floating point numbers)
> - Bit-fields
> - alloca()
> - Variable-length arrays
> - Compound literals
> - Thread-local variables
> - Atomic variables
> - Common symbols
> - Designated initializers
> - L, u, U and u8 string literals
> - Functions that take or return structs as values, as specified by the
>   x86-64 SystemV ABI
> 
> chibicc does not support complex numbers, K&R-style function prototypes
> and GCC-style inline assembly. Digraphs and trigraphs are intentionally
> left out.
> 
> chibicc outputs a simple but nice error message when it finds an error in
> source code.
> 
> There's no optimization pass. chibicc emits terrible code which is probably
> twice or more slower than GCC's output. I have a plan to add an
> optimization pass once the frontend is done.
> 
> I'm using Ubuntu 20.04 for x86-64 as a development platform. I made a
> few small changes so that chibicc works on Ubuntu 18.04, Fedora 32 and
> Gentoo 2.6, but portability is not my goal at this moment. It may or
> may not work on systems other than Ubuntu 20.04.
> 
> ## Internals
> 
> chibicc consists of the following stages:
> 
> - Tokenize: A tokenizer takes a string as an input, breaks it into a list
>   of tokens and returns them.
> 
> - Preprocess: A preprocessor takes as an input a list of tokens and output
>   a new list of macro-expanded tokens. It interprets preprocessor
>   directives while expanding macros.
> 
> - Parse: A recursive descendent parser constructs abstract syntax trees
>   from the output of the preprocessor. It also adds a type to each AST
>   node.
> 
> - Codegen: A code generator emits an assembly text for given AST nodes.

I haven't dived deep into the intricacies of C compilers. Though I downloaded the [n2310.pdf](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n2310.pdf), 
I never got around to reading it thoroughly. This project is a crude imitation of chibicc. I owe thanks to Rui for his clear project submissions, 
which allowed me to experience the details and process of building a somewhat functional C compiler. Admittedly, compared to a real compiler, 
it's just a 'small C compiler', but we've come quite a long way. Perhaps next up will be [acwj](https://github.com/DoctorWkt/acwj/) or [Writing a C Compiler](https://norasandler.com/book/), 
they're also on my reading list.






