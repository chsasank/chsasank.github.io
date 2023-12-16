---
layout: post
title: Scheme Compiler in Incremental Steps
author: Sasank Chilamkurthy
twitter_image: "https://miro.medium.com/v2/resize:fit:500/1*Qbm5_d5EYIbYa1-jN4JmSg.jpeg"
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

## Integers

Let's start with integers or fixnums. We'll take help from `gcc` to find the relevant assembly code.

```c
// ctest.c
int scheme_entry(){
    return 42;
}
```

```bash
$ gcc -O3 --omit-frame-pointer -S ctest.c
$ cat ctest.s
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