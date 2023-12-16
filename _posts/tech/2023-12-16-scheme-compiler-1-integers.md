---
layout: post
title: "Scheme Compiler in Incremental Steps: Compiling Integers"
author: Sasank Chilamkurthy
twitter_image: "https://1010jms.github.io/images/scheme.png"
---

I somehow came to love compilers. May be it was my experience with PyTorch - after all it is some sort of interpreter/compiler combo which executed python code on GPUs. I figured that compilers are an underappreciated part of what makes computers tick. Compilers enabled first human-computer interface: humans can now talk to computers in a more intelligible language than 0/1s. 

Compilers are considered arcane pieces of software. They are purely symbolic pieces of software which translate programs from one language to another. Usually translation happens from higher level languages like C++ to lower level assembly language but this doesn't have to be the case. What makes compilers so hard is that you have to preserve the semantics of the program between source and target languages.

Compilers are usually written in passes. What this means is that each pass of the compiler transforms programs into a representation that is input to next pass. It looks something like this:

```scheme
; passes are functions that take in a representation
; and return another representations
(define compiler-passes
  (list lexical-analysis syntax-analysis 
        semantic-analysis dead-code-removal code-generation))

(define (compile-program program)
    (compile-with-passes program compiler-passes))

(define (compile-with-passes program-representation passes)
    (if (null? passes)
        program-representation
        (compile-with-passes
            ((car passes) program-representation)
            (cdr passes))))
```

* Lexical analysis convert program text into tokens
* Syntax analysis transforms tokens into abstract syntax tree
* Semantic analysis adds meaning to language constructs
* Dead code removal is a type of optimization that removes code which is not reached by the control flow
* Code generation finally generates code in target language.

These are common passes but there can be a lot of passes or very few passes. Nano pass framework takes above to extreme by creating a lot of ultra small passes (in the order of 50 passes).

However modular this makes the compiler construction, such frameworks are a terrible way to teach how compilers work. This is because you never know see the working compiler until you wrote all the passes. Alternative approach to teach compilers is presented in "[An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf)" by Abdulaziz Ghuloum. In this approach, we incrementally compile features of the language starting from the simplest expressions to more complicated ones. We get sense of achievement at every step!

In this tutorial, we follow this approach to compile a subset of scheme. We will also use scheme as our implementation language.

### Compiling Integers

Let's start with integers or fixnums. We'll take help from `gcc` to find the relevant assembly code.

```c
// test.c
int scheme_entry(){
    return 42;
}
```

```bash
$ gcc -O3 --omit-frame-pointer -S test.c
$ cat test.s
	.file	"test.c"
	.text
	.p2align 4
	.globl	scheme_entry
	.type	scheme_entry, @function
scheme_entry:
.LFB0:
	.cfi_startproc
	movl	$42, %eax
	ret
	.cfi_endproc
.LFE0:
	.size	scheme_entry, .-scheme_entry
	.ident	"GCC: (Debian 12.2.0-14) 12.2.0"
	.section	.note.GNU-stack,"",@progbits

```

In the assembly code, you can see `scheme_entry` is compiled to following two instructions:

```
movl	$42, %eax
ret
```

Rest of the assembly code is boilerplate. The register %eax serves as the return value register. Simple to emit this in scheme:

```scheme
(define (compile-program x)
    (emit "movl $~a, %eax" x)
    (emit "ret"))
```

### Linker and Runtime

To execute this we need to create linker or runtime that takes above instructions and runs it. To do that let's create a `runtime.c` that takes in a `scheme_entry` function and runs it:

```c
//runtime.c
#include <stdio.h>

int main(int argc, char** argv){
    printf("%d\n", scheme_entry());
    return 0;
}
```

Now we need to create assembly file containing `scheme_entry` function. Then that assembly file needs to be linked with above runtime. We'll then get executable which can be run. Following scheme script does the job:

```scheme
; linker.scm
(load "./compiler.scm")
(use-modules (ice-9 textual-ports))

(define output-file-param (make-parameter #f))

(define (emit format-string . args)
  ; Format the assembly code using format
  (apply format (output-file-param) format-string args)
  (newline (output-file-param)))

(define (build x)
    (define output-file (open-output-file "/tmp/scheme_entry.s"))
    (output-file-param output-file)
    (display (string-append 
        ".text\n"
        ".p2align 4\n"
        ".globl	scheme_entry\n"        
        ".type	scheme_entry, @function\n"
        "scheme_entry:\n")
        output-file)

    (compile-program x)

    (display (string-append 
        ".LFE0:\n"
        ".size	scheme_entry, .-scheme_entry\n"
        ".section	.note.GNU-stack,\"\",@progbits\n")
        output-file)
    
    (close-output-port output-file))

(define (run x)
    (build x)
    (system "gcc -w runtime.c /tmp/scheme_entry.s -o /tmp/scheme_entry")
    (system "/tmp/scheme_entry > /tmp/scheme_entry.out")
    (call-with-input-file "/tmp/scheme_entry.out"
        get-string-all))
```

To run this:

```
$ guile 
GNU Guile 3.0.8
scheme@(guile-user)> (load "linker.scm")
scheme@(guile-user)> (run 2)
$1 = "2\n"
```

### Testing

Since we're adding features incrementally, we want to write tests to ensure that later features don't break the already written code. `run` function from `linker.scm` returns the output of the linked executable. We just have to compare this to expected result for a set of known cases. Here's a simple test driver:

```scheme
; tests.scm

(load "linker.scm")

(define (run-test expr expected-result)
    (let ((result (run expr)))
        (if (string=? result expected-result)
            (begin (display expr)
                (display ": passed\n"))
            (error 'failed expr))))

; integers
(run-test 0 "0\n")
(run-test 1 "1\n")
(run-test -1 "-1\n")
(run-test 10 "10\n")
(run-test -10 "-10\n")
(run-test 2736 "2736\n")
(run-test -2736 "-2736\n")
(run-test 536870911 "536870911\n")
(run-test -536870912 "-536870912\n")
```

Execute these tests with 

```bash
$ guile tests.scm 
0: passed
1: passed
-1: passed
10: passed
-10: passed
2736: passed
-2736: passed
536870911: passed
-536870912: passed
```

And that's all for today folks. We have generated assembly instructions for compiling a integer, linked to a simple runtime and ran it. In the next part, we'll continue with compiling constants and other features.