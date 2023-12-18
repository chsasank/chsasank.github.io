---
layout: post
title: "Scheme Compiler in Incremental Steps: Binary Primitives"
author: Sasank Chilamkurthy
twitter_image: "https://1010jms.github.io/images/scheme.png"
---

In the previous post, we added unary primitives like `add1` and `null?` to our language. In this post, let's implement binary primitives like `+`, `*`, and `char<?`. We cannot, in general, use a single register because evaluating one argument may overwrite the value from other argument. So we will use stack to save immediate value of computations.

For example code for `(+ e0 e1)` is achieved by 
1. Emit code for e0
2. Emit instruction for saving value in %eax on the stack
3. Emit code for e1
4. Add the value of %eax to the value on stack

The stack is arranged as a contiguous array of memory locations. `%rsp` register contains pointer to the base of the stack, which contains the return point. We shouldn't modify return point (`%rsp`) but can use the locations above the return point (`-8(%rsp)`, `-16(%rsp)`, etc) to hold our intermediate values. Note that we're using a 64 bit machine and word size is therefore 8.

In order to guarantee that values stored in stack are never overwritten, our code emitters will maintain the value of stack index. Stack index is a negative number that points to first stack location that is free. We initialize it to -8 and decremented by 8 because 8 bytes is our word size.

![Stack](/assets/images/scheme_compiler/stack.png)

## Infrastructure Upgrade: Stack

Since we're now adding stack to our programming language, we need to upgrade our infrastructure yet again. In our `runtime.c`, we need to allocate sufficiently large memory space for our stack. We will surround this block of memory with two protected pages so that the program crashes if it tries to access the memory locations outside these. A pointer to base of this stack is passed to `scheme_entry` procedure.

Let's start with `runtime.c`. Let's use [`mmap`](https://en.wikipedia.org/wiki/Mmap) to create virtual address space for our stack. We'll use [`mprotect`](https://man7.org/linux/man-pages/man2/mprotect.2.html) to ensure nothing is written to our protected pages. I have refactored code a bit by separating printing part of the runtime into a function. Our `scheme_entry` function will now take `stack_base` as an argument. This will be used to set `%rsp` to this location. Refer to above figure:

```c
// runtime.c
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>

#define fixnum_shift 2
#define fixnum_mask 0b11
#define fixnum_tag  0b00
#define char_shift  8
#define char_mask   0b11111111
#define char_tag    0b00001111
#define bool_shift  7
#define bool_mask   0b1111111
#define bool_tag    0b0011111
#define null_val 0b00101111

/*Print values as tagged pointer representation*/
static void print_val(int val){
    if ((val & fixnum_mask) == fixnum_tag){
        printf("%d\n", val >> fixnum_shift);
    } else if ((val & char_mask) == char_tag){
        printf("#\\%c\n", val >> char_shift);
    } else if ((val & bool_mask) == bool_tag){
        if (val >> bool_shift == 0){
            printf("#f\n");
        } else {
            printf("#t\n");
        }
    }
    else if (val == null_val){
        printf("()\n");
    }
}

/*Allocate protected stack space and return pointer to
top of the stack*/ 
static char* allocate_protected_space(int size){
    // get memory page size (libc)
    int page = getpagesize();
    int status;
    // align size to multiples of page size
    int aligned_size = ((size + page -1) / page) * page;

    // create new protected virtual address space
    // with extra protection page above and below 
    char *p = mmap(0, aligned_size + 2 * page,
        PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE,
        0, 0);
    // exit if mmap failed
    if (p == MAP_FAILED){
        perror("mmap failed");
        exit(1);
    }
    // protect top of our stack
    status = mprotect(p, page, PROT_NONE);
    if (status != 0){
        perror("mprotect failed");
        exit(1);
    }

    // protect base of our stack
    status = mprotect(p + page + aligned_size, page, PROT_NONE);
    if (status != 0){
        perror("mprotect failed");
        exit(1);
    }

    // pointer to top of stack
    return (p + page);
}

/* Clear stack we created */
static char* deallocate_protected_space(char *p, int size){
    int page = getpagesize();
    int status;
    int aligned_size = ((size + page - 1) / page) * page;
    // remove virtual address space we created before
    status = munmap(p - page, aligned_size + 2 * page);
    if (status != 0){
        perror("munmap failed");
        exit(1);
    }
}

int main(int argc, char** argv){
    int stack_size = (16 * 4096);   /* holds 16k cells*/
    char *stack_top = allocate_protected_space(stack_size);
    char *stack_base = stack_top + stack_size;
    int val = scheme_entry(stack_base);
    print_val(val);
    deallocate_protected_space(stack_top, stack_size);
    return 0;
}
```

Now let's turn to `scheme_entry`. According to x86-64 calling convention `%rdi` stores the first argument (`stack_base` in our case). We need to set the `%rsp` register to newly allocated memory space (address in `rdi`). However, we'll want to reset `%rsp` to old value when done with our program. So we'll use a scratch register `%rcx` to the save original value of `%rsp`. Here's how the assembly program will look like:

```asm
scheme_entry:
    mov %rsp, %rcx      ; save current stack pointer
    mov %rdi, %rsp      ; set stack pointer to allocated space
    call L_scheme_entry ; where the real action happens
    mov %rcx, %rsp      ; set stack pointer to original value
    ret
```

To achieve this, we need to change our linker:

```scheme
; edit in linker.asm
(define (build x)
    (define output-file (open-output-file "/tmp/scheme_entry.s"))
    (output-file-param output-file)
    (display (string-append 
        ".text\n"
        ".p2align 4\n"
        ".globl	scheme_entry\n"        
        ".type	scheme_entry, @function\n"
        "scheme_entry:\n"
        "mov %rsp, %rcx\n"
        "mov %rdi, %rsp\n"
        "call L_scheme_entry\n"
        "mov %rcx, %rsp\n"
        "ret\n"
        "L_scheme_entry:\n")
        output-file)

    (compile-program x)

    (display (string-append 
        ".LFE0:\n"
        ".size	scheme_entry, .-scheme_entry\n"
        ".section	.note.GNU-stack,\"\",@progbits\n")
        output-file)
    
    (close-output-port output-file))
```

Finally we need to change our `compiler.asm` to include stack index `si` in `emit-expr` and `define-primitive`.

```scheme
; edit in compiler.asm
(define word-size 8)

; add si to primitive macro
(define-syntax define-primitive
    (syntax-rules ()
        ((_ (prim-name si arg* ...) b b* ...)
            (begin
                (set-symbol-property! 'prim-name '*is-prim* #t)
                (set-symbol-property! 'prim-name '*arg-count* 
                    (length '(arg* ...)))
                (set-symbol-property! 'prim-name '*emitter*
                    (lambda (si arg* ...) b b* ...))))))

(define (compile-program x)
    ; initialize stack index - word-size so as not to 
    ; overwrite return address
    (emit-expr (- word-size) x)
    (emit "ret"))


(define (emit-expr si expr)
    (cond
        ((immediate? expr) (emit-immediate expr))
        ((primcall? expr) (emit-primcall si expr))
        (else (error "syntax error"))))

(define (emit-primcall si expr)
    (let ((prim (car expr))
          (args (cdr expr)))
        (apply (primitive-emitter prim) si args)))

(define (emit-immediate expr)
    (emit "movl $~a, %eax" (immediate-rep expr)))

; add si to all primitives definitions
(define-primitive (add1 si arg)
    ; add si to all emit-expr
    (emit-expr si arg)
    (emit "addl $~a, %eax" (immediate-rep 1)))

(define-primitive (sub1 si arg)
    (emit-expr si arg)
    (emit "subl $~a, %eax" (immediate-rep 1)))

; repeat for all primitives
```

Test our infrastructure by running old tests:

```bash
$ guile tests.scm
...
(boolean? (integer? (boolean? 0))): passed
(not #t): passed
(not #f): passed
(not 15): passed
(not ()): passed
(not A): passed
(not (not #t)): passed
(not (not #f)): passed
(not (not 15)): passed
(not (integer? 15)): passed
(not (integer? #f)): passed
```

## Add Binary Primitives

With that infrastructure, adding binary primitives should be breeze. We will follow the algorithm stated at the top of the post. Intermediate values of arguments will be saved into stack. Stack index is used to track stack size at the compile time.

Let's start with `+`.

```scheme
; binary primitives
(define-primitive (+ si arg1 arg2)
    (emit-expr si arg1)
    
    ; save result of arg1 in stack
    (emit "movl %eax, ~a(%rsp)" si)

    ; move stack index by a word so that 
    ; above is not overwritten
    (emit-expr (- si word-size) arg2)

    ; add result from stack with arg2 result
    ; this works because of integer tag = b00
    (emit "addl ~a(%rsp), %eax" si))
```

Rest of the primitives are also quite straight forward.

```scheme
(define-primitive (- si arg1 arg2)
    (emit-expr si arg2)
    (emit "movl %eax, ~a(%rsp)" si)
    (emit-expr (- si word-size) arg1)
    (emit "subl ~a(%rsp), %eax" si))

(define-primitive (* si arg1 arg2)
    (emit-expr si arg1)
    (emit "movl %eax, ~a(%rsp)" si)
    (emit-expr (- si word-size) arg2)
    ; remove b00 from int for one of the args
    (emit "shrl $~a, %eax" fixnum-shift)
    (emit "imull ~a(%rsp), %eax" si))

(define-primitive (= si arg1 arg2)
    (emit-expr si arg1)
    (emit "movl %eax, ~a(%rsp)" si)
    (emit-expr (- si word-size) arg2)
    (emit "cmpl ~a(%rsp), %eax" si)
    (zeroflag-to-bool))

(define-primitive (< si arg1 arg2)
    (emit-expr si arg1)
    (emit "movl %eax, ~a(%rsp)" si)
    (emit-expr (- si word-size) arg2)
    (emit "cmpl %eax, ~a(%rsp)" si)
    
    (emit "movl $0, %eax")
    ; set lower byte of eax register to 0/1 if cmpl is less than
    (emit "setl %al")
    ; convert 0/1 in eax to bool
    (emit "sall $~a, %eax" bool-shift)
    (emit "orl $~a, %eax" bool-tag))

(define-primitive (<= si arg1 arg2)
    (emit-expr si arg1)
    (emit "movl %eax, ~a(%rsp)" si)
    (emit-expr (- si word-size) arg2)
    (emit "cmpl %eax, ~a(%rsp)" si)
    
    (emit "movl $0, %eax")
    (emit "setle %al")
    (emit "sall $~a, %eax" bool-shift)
    (emit "orl $~a, %eax" bool-tag))

(define-primitive (> si arg1 arg2)
    (emit-expr si arg1)
    (emit "movl %eax, ~a(%rsp)" si)
    (emit-expr (- si word-size) arg2)
    (emit "cmpl %eax, ~a(%rsp)" si)
    
    (emit "movl $0, %eax")
    (emit "setg %al")
    (emit "sall $~a, %eax" bool-shift)
    (emit "orl $~a, %eax" bool-tag))

(define-primitive (>= si arg1 arg2)
    (emit-expr si arg1)
    (emit "movl %eax, ~a(%rsp)" si)
    (emit-expr (- si word-size) arg2)
    (emit "cmpl %eax, ~a(%rsp)" si)
    
    (emit "movl $0, %eax")
    (emit "setge %al")
    (emit "sall $~a, %eax" bool-shift)
    (emit "orl $~a, %eax" bool-tag))
```

Finally let's add tests for these primitives:

```scheme
; add in tests.scm

; +
(run-test '(+ 1 2) "3\n")
(run-test '(+ 1 -2) "-1\n")
(run-test '(+ -1 2) "1\n")
(run-test '(+ -1 -2) "-3\n")
(run-test '(+ 536870911 -1) "536870910\n")
(run-test '(+ 536870910 1) "536870911\n")
(run-test '(+ -536870912 1) "-536870911\n")
(run-test '(+ -536870911 -1) "-536870912\n")
(run-test '(+ 536870911 -536870912) "-1\n")
(run-test '(+ 1 (+ 2 3)) "6\n")
(run-test '(+ 1 (+ 2 -3)) "0\n")
(run-test '(+ 1 (+ -2 3)) "2\n")
(run-test '(+ 1 (+ -2 -3)) "-4\n")
(run-test '(+ -1 (+ 2 3)) "4\n")
(run-test '(+ -1 (+ 2 -3)) "-2\n")
(run-test '(+ -1 (+ -2 3)) "0\n")
(run-test '(+ -1 (+ -2 -3)) "-6\n")
(run-test '(+ (+ 1 2) 3) "6\n")
(run-test '(+ (+ 1 2) -3) "0\n")
(run-test '(+ (+ 1 -2) 3) "2\n")
(run-test '(+ (+ 1 -2) -3) "-4\n")
(run-test '(+ (+ -1 2) 3) "4\n")
(run-test '(+ (+ -1 2) -3) "-2\n")
(run-test '(+ (+ -1 -2) 3) "0\n")
(run-test '(+ (+ -1 -2) -3) "-6\n")
(run-test '(+ (+ (+ (+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6) 7) 8) 9) "45\n")
(run-test '(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 9)))))))) "45\n")

; -
(run-test '(- 1 2) "-1\n")
(run-test '(- 1 -2) "3\n")
(run-test '(- -1 2) "-3\n")
(run-test '(- -1 -2) "1\n")
(run-test '(- 536870910 -1) "536870911\n")
(run-test '(- 536870911 1) "536870910\n")
(run-test '(- -536870911 1) "-536870912\n")
(run-test '(- -536870912 -1) "-536870911\n")
(run-test '(- 1 536870911) "-536870910\n")
(run-test '(- -1 536870911) "-536870912\n")
(run-test '(- 1 -536870910) "536870911\n")
(run-test '(- -1 -536870912) "536870911\n")
(run-test '(- 536870911 536870911) "0\n")
(run-test '(- -536870911 -536870912) "1\n")
(run-test '(- 1 (- 2 3)) "2\n")
(run-test '(- 1 (- 2 -3)) "-4\n")
(run-test '(- 1 (- -2 3)) "6\n")
(run-test '(- 1 (- -2 -3)) "0\n")
(run-test '(- -1 (- 2 3)) "0\n")
(run-test '(- -1 (- 2 -3)) "-6\n")
(run-test '(- -1 (- -2 3)) "4\n")
(run-test '(- -1 (- -2 -3)) "-2\n")
(run-test '(- 0 (- -2 -3)) "-1\n")
(run-test '(- (- 1 2) 3) "-4\n")
(run-test '(- (- 1 2) -3) "2\n")
(run-test '(- (- 1 -2) 3) "0\n")
(run-test '(- (- 1 -2) -3) "6\n")
(run-test '(- (- -1 2) 3) "-6\n")
(run-test '(- (- -1 2) -3) "0\n")
(run-test '(- (- -1 -2) 3) "-2\n")
(run-test '(- (- -1 -2) -3) "4\n")
(run-test '(- (- (- (- (- (- (- (- 1 2) 3) 4) 5) 6) 7) 8) 9) "-43\n")
(run-test '(- 1 (- 2 (- 3 (- 4 (- 5 (- 6 (- 7 (- 8 9)))))))) "5\n")

; *
(run-test '(* 2 3) "6\n")
(run-test '(* 2 -3) "-6\n")
(run-test '(* -2 3) "-6\n")
(run-test '(* -2 -3) "6\n")
(run-test '(* 536870911 1) "536870911\n")
(run-test '(* 536870911 -1) "-536870911\n")
(run-test '(* -536870912 1) "-536870912\n")
(run-test '(* -536870911 -1) "536870911\n")
(run-test '(* 2 (* 3 4)) "24\n")
(run-test '(* (* 2 3) 4) "24\n")
(run-test '(* (* (* (* (* 2 3) 4) 5) 6) 7) "5040\n")
(run-test '(* 2 (* 3 (* 4 (* 5 (* 6 7))))) "5040\n")

(run-test '(= 12 13) "#f\n")
(run-test '(= 12 12) "#t\n")
(run-test '(= 16 (+ 13 3)) "#t\n")
(run-test '(= 16 (+ 13 13)) "#f\n")
(run-test '(= (+ 13 3) 16) "#t\n")
(run-test '(= (+ 13 13) 16) "#f\n")

(run-test '(< 12 13) "#t\n")
(run-test '(< 12 12) "#f\n")
(run-test '(< 13 12) "#f\n")
(run-test '(< 16 (+ 13 1)) "#f\n")
(run-test '(< 16 (+ 13 3)) "#f\n")
(run-test '(< 16 (+ 13 13)) "#t\n")
(run-test '(< (+ 13 1) 16) "#t\n")
(run-test '(< (+ 13 3) 16) "#f\n")
(run-test '(< (+ 13 13) 16) "#f\n")


(run-test '(<= 12 13)  "#t\n")
(run-test '(<= 12 12)  "#t\n")
(run-test '(<= 13 12)  "#f\n")
(run-test '(<= 16 (+ 13 1))  "#f\n")
(run-test '(<= 16 (+ 13 3))  "#t\n")
(run-test '(<= 16 (+ 13 13))  "#t\n")
(run-test '(<= (+ 13 1) 16)  "#t\n")
(run-test '(<= (+ 13 3) 16)  "#t\n")
(run-test '(<= (+ 13 13) 16)  "#f\n")


(run-test '(> 12 13) "#f\n")
(run-test '(> 12 12) "#f\n")
(run-test '(> 13 12) "#t\n")
(run-test '(> 16 (+ 13 1)) "#t\n")
(run-test '(> 16 (+ 13 3)) "#f\n")
(run-test '(> 16 (+ 13 13)) "#f\n")
(run-test '(> (+ 13 1) 16) "#f\n")
(run-test '(> (+ 13 3) 16) "#f\n")
(run-test '(> (+ 13 13) 16) "#t\n")


(run-test '(>= 12 13) "#f\n")
(run-test '(>= 12 12) "#t\n")
(run-test '(>= 13 12) "#t\n")
(run-test '(>= 16 (+ 13 1)) "#t\n")
(run-test '(>= 16 (+ 13 3)) "#t\n")
(run-test '(>= 16 (+ 13 13)) "#f\n")
(run-test '(>= (+ 13 1) 16) "#f\n")
(run-test '(>= (+ 13 3) 16) "#t\n")
(run-test '(>= (+ 13 13) 16) "#t\n")
```

And run tests:

```bash
$ guile tests.scm
...
(>= 12 13): passed
(>= 12 12): passed
(>= 13 12): passed
(>= 16 (+ 13 1)): passed
(>= 16 (+ 13 3)): passed
(>= 16 (+ 13 13)): passed
(>= (+ 13 1) 16): passed
(>= (+ 13 3) 16): passed
(>= (+ 13 13) 16): passed
```

And that's all for today folks! As you can see, majority of the work involved is creating infrastructure for stack. Once infrastructure is created, writing individual primitives is quite easy.

Working code at the end of this step can be found at my [Github repo](https://github.com/chsasank/scheme-incremental-compiler) with tag [`step_4_binary_primitives`](https://github.com/chsasank/scheme-incremental-compiler/releases/tag/step_4_binary_primitives)
