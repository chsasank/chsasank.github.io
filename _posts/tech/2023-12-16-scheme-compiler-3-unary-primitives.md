---
layout: post
title: "Scheme Compiler in Incremental Steps: Unary Primitives"
author: Sasank Chilamkurthy
twitter_image: "https://1010jms.github.io/images/scheme.png"
---

In the previous step, we added immediate constants for integers, characters, booleans and empty list. We now extend the language to include calls to primitives that accept one argument. Let's start with `add1` and `sub1` which are increment and decrement operators respectively for integers. We'll then add `integer->char` and `char->integer` primitives. Finally we will implement predicates `null?`, `zero?` and `not`.

## Infrastructure Upgrade

As our language is growing, we need to upgrade our infrastructure. We'll create a macro for adding new primitives. We'll store the following properties in a proplist corresponding to symbol representing the primitive:

* `*is-prim*`: Is it a primitive?
* `*arg-count*`: Number of arguments for the primitive
* `*emitter`: Procedure that handles emitting the code for the primitive

We will also add a few helper functions for primitives.

```scheme
; add in compiler.scm
; infra for primitives 
(define-syntax define-primitive
    (syntax-rules ()
        ((_ (prim-name arg* ...) b b* ...)
            (begin
                (set-symbol-property! 'prim-name '*is-prim* #t)
                (set-symbol-property! 'prim-name '*arg-count* 
                    (length '(arg* ...)))
                (set-symbol-property! 'prim-name '*emitter*
                    (lambda (arg* ...) b b* ...))))))

(define (primitive? x)
    ; is x a primitve
    (and (symbol? x) (symbol-property x '*is-prim*)))

(define (primitive-emitter x)
    ; get primitve emitter of x
    (or (symbol-property x '*emitter*) (error "not primitive"))) 

(define (primcall? expr)
    ; is expr a primitive call
    (and (pair? expr) (primitive? (car expr))))
```

We will then refactor our `compile-program` to `emit-expr` and emitting a `ret`. This is so that we can use `emit-expr` recursively to compile arguments to our primitives. We will also create separate function for emitting for immediates and primitives:

```scheme
; replace compile-program in compiler.scm
; cases for compiler
(define (compile-program x)    
    (emit-expr x)
    (emit "ret"))

(define (emit-expr expr)
    (cond
        ((immediate? expr) (emit-immediate expr))
        ((primcall? expr) (emit-primcall expr))
        (else (error "syntax error"))))

(define (emit-primcall expr)
    (let ((prim (car expr))
          (args (cdr expr)))
        (apply (primitive-emitter prim) args)))

(define (emit-immediate expr)
    (emit "movl $~a, %eax" (immediate-rep expr)))
```

## Increment and Decrement Primitives

We're finally ready to add primitives `add1` (increment by 1) and `subl1` (decrement by 1). We will use `addl`/`subl` machine instructions

```scheme
; in compiler.scm
(define-primitive (add1 arg)
    (emit-expr arg)
    (emit "addl $~a, %eax" (immediate-rep 1)))

(define-primitive (sub1 arg)
    (emit-expr arg)
    (emit "subl $~a, %eax" (immediate-rep 1)))
```

This works because `%eax` stores the return value of `arg` expression. If we've had two arguments for our primitive, a single register would not work. We'll take that case up in next step.

Add the following tests to `tests.scm` to verify everything works

```scheme
; add to tests.scm
; add1
(run-test '(add1 0) "1\n")
(run-test '(add1 -1) "0\n")
(run-test '(add1 1) "2\n")
(run-test '(add1 -100) "-99\n")
(run-test '(add1 536870910) "536870911\n")
(run-test '(add1 -536870912) "-536870911\n")
(run-test '(add1 (add1 0)) "2\n")
(run-test '(add1 (add1 (add1 (add1 (add1 (add1 12)))))) "18\n")

; sub1
(run-test '(sub1 0) "-1\n")
(run-test '(sub1 -1) "-2\n")
(run-test '(sub1 1) "0\n")
(run-test '(sub1 -100) "-101\n")
(run-test '(sub1 (add1 0)) "0\n")
```

Verify tests using

```
$ guile tests.scm 
...
(add1 0): passed
(add1 -1): passed
(add1 1): passed
(add1 -100): passed
(add1 536870910): passed
(add1 -536870912): passed
(add1 (add1 0)): passed
(add1 (add1 (add1 (add1 (add1 (add1 12)))))): passed
(sub1 0): passed
(sub1 -1): passed
(sub1 1): passed
(sub1 -100): passed
(sub1 (add1 0)): passed
```


## Type Conversion


Let's add type conversion primitives `integer->char` and `char->integer`. To do this we will use bitshift instructions from x86:

* `shll`: Left logical/bitwise shift
* `shrl`: Right logical/bitwise shift
* `orl`: Bitwise logical or


To convert integer to char, left shift by 8 - 2 = 6 bits. Then add character tag. For converting char to integer, just right shift by 6 bits. No need to add any tag because first 2 bits of char tag are anyway `00`.

```scheme
; add to compiler.scm
(define-primitive (integer->char arg)
    (emit-expr arg)
    (emit "shll $~a, %eax" (- char-shift fixnum-shift))
    (emit "orl $~a, %eax" char-tag))

(define-primitive (char->integer arg)
    (emit-expr arg)
    (emit "shrl $~a, %eax" (- char-shift fixnum-shift)))
```

Let's add tests and verify if everything works:

```scheme
; add in tests.scm
; integer->char, char->integer
(run-test '(integer->char 65) "#\\A\n")
(run-test '(integer->char 97) "#\\a\n")
(run-test '(integer->char 122) "#\\z\n")
(run-test '(integer->char 90) "#\\Z\n")
(run-test '(integer->char 48) "#\\0\n")
(run-test '(integer->char 57) "#\\9\n")
(run-test '(char->integer #\A) "65\n")
(run-test '(char->integer #\a) "97\n")
(run-test '(char->integer #\z) "122\n")
(run-test '(char->integer #\Z) "90\n")
(run-test '(char->integer #\0) "48\n")
(run-test '(char->integer #\9) "57\n")
(run-test '(char->integer (integer->char 12)) "12\n")
(run-test '(integer->char (char->integer #\x)) "#\\x\n")
```

Run test cases:
```
$ guile tests.scm
...
(integer->char 65): passed
(integer->char 97): passed
(integer->char 122): passed
(integer->char 90): passed
(integer->char 48): passed
(integer->char 57): passed
(char->integer A): passed
(char->integer a): passed
(char->integer z): passed
(char->integer Z): passed
(char->integer 0): passed
(char->integer 9): passed
(char->integer (integer->char 12)): passed
(integer->char (char->integer x)): passed
```

## Predicates

We will now implement predicates `zero?`, `null?` and `not`. These are not as simple as above. We need to use more x86 instructions/registers for comparison:

* `cmpl`: compare operands and set zero flag if equal
* `cmp`: Same as `cmpl` but for bytes
* `sete`: Sets destination if zero flag is set from previous instructions
* `%al`: Lower 8 bits of register `%eax`

To check for zero, we will use following instructions:

```assembly
; compare 0 to result and set zero flag if true
cmpl $0, %eax
; make eax 0
movl $0, %eax
; set lower byte of eax register to 0/1 based on zeroflag
sete %al
; convert 0/1 in eax to bool
sall $7, %eax
orl $63, %eax
```

Of these, instructions after the first one create boolean from whether zero bit is set. This routine is useful for other predicates too. So we'll extract it into a procedure:

```
(define (zeroflag-to-bool)
    ; convert zeroflag set by cmp to boolean
    ; used in other primitives
    ; make eax 0
    (emit "movl $0, %eax")
    ; set lower byte of eax register to 0/1 based on zeroflag
    (emit "sete %al")
    ; convert 0/1 in eax to bool
    (emit "sall $~a, %eax" bool-shift)
    (emit "orl $~a, %eax" bool-tag))
```

Now writing predicates become quite easy

```scheme
; in compiler.asm
(define-primitive (zero? arg)
    (emit-expr arg)
    ; compare 0 to result and set zero flag if true
    (emit "cmpl $0, %eax")
    (zeroflag-to-bool))

(define-primitive (null? arg)
    (emit-expr arg)
    ; compare null value bits to result and set zero flag if true
    (emit "cmpl $~a, %eax" null-val)
    (zeroflag-to-bool))

(define-primitive (not arg)
    ; return true only if arg is #f
    (emit-expr arg)
    ; compare immediate rep of #f to result
    ; set zero flag if true
    (emit "cmpl $~a, %eax" (immediate-rep #f))
    (zeroflag-to-bool))

(define-primitive (integer? arg)
    (emit-expr arg)
    ; apply fixnum mask (1 bits fixnum-shift times)
    (emit "and $~s, %al" (- (ash 1 fixnum-shift) 1))
    (emit "cmp $~s, %al" #b00)
    (zeroflag-to-bool))

(define-primitive (char? arg)
    (emit-expr arg)
    ; apply fixnum mask (1 bits char-shift times)
    (emit "and $~s, %al" (- (ash 1 char-shift) 1))
    (emit "cmp $~s, %al" char-tag)
    (zeroflag-to-bool))

(define-primitive (boolean? arg)
    (emit-expr arg)
    ; apply fixnum mask (1 bits bool-shift times)
    (emit "and $~s, %al" (- (ash 1 bool-shift) 1))
    (emit "cmp $~s, %al" bool-tag)
    (zeroflag-to-bool))
```

Add following test cases:

```scheme
; add in tests.scm

; zero?
(run-test '(zero? 0) "#t\n")
(run-test '(zero? 1) "#f\n")
(run-test '(zero? -1) "#f\n")
(run-test '(zero? (sub1 1)) "#t\n")
; null?
(run-test '(null? ()) "#t\n")
(run-test '(null? #f) "#f\n")
(run-test '(null? #t) "#f\n")
(run-test '(null? (null? ())) "#f\n")
(run-test '(null? #\a) "#f\n")
(run-test '(null? 0) "#f\n")
(run-test '(null? -10) "#f\n")
(run-test '(null? 10) "#f\n")
; integer?
(run-test '(integer? 0) "#t\n")
(run-test '(integer? 1) "#t\n")
(run-test '(integer? -1) "#t\n")
(run-test '(integer? 37287) "#t\n")
(run-test '(integer? -23873) "#t\n")
(run-test '(integer? 536870911) "#t\n")
(run-test '(integer? -536870912) "#t\n")
(run-test '(integer? #t) "#f\n")
(run-test '(integer? #f) "#f\n")
(run-test '(integer? ()) "#f\n")
(run-test '(integer? #\Q) "#f\n")
(run-test '(integer? (integer? 12)) "#f\n")
(run-test '(integer? (integer? #f)) "#f\n")
(run-test '(integer? (integer? #\A)) "#f\n")
(run-test '(integer? (char->integer #\r)) "#t\n")
(run-test '(integer? (integer->char 12)) "#f\n")
; char?
(run-test '(char? #\a) "#t\n")
(run-test '(char? #\Z) "#t\n")
(run-test '(char? #t) "#f\n")
(run-test '(char? #f) "#f\n")
(run-test '(char? ()) "#f\n")
(run-test '(char? (char? #t)) "#f\n")
(run-test '(char? 0) "#f\n")
(run-test '(char? 23870) "#f\n")
(run-test '(char? -23789) "#f\n")
; boolean?
(run-test '(boolean? #t) "#t\n")
(run-test '(boolean? #f) "#t\n")
(run-test '(boolean? 0) "#f\n")
(run-test '(boolean? 1) "#f\n")
(run-test '(boolean? -1) "#f\n")
(run-test '(boolean? ()) "#f\n")
(run-test '(boolean? #\a) "#f\n")
(run-test '(boolean? (boolean? 0)) "#t\n")
(run-test '(boolean? (integer? (boolean? 0))) "#t\n")
; not
(run-test '(not #t) "#f\n")
(run-test '(not #f) "#t\n")
(run-test '(not 15) "#f\n")
(run-test '(not ()) "#f\n")
(run-test '(not #\A) "#f\n")
(run-test '(not (not #t)) "#t\n")
(run-test '(not (not #f)) "#f\n")
(run-test '(not (not 15)) "#t\n")
(run-test '(not (integer? 15)) "#f\n")
(run-test '(not (integer? #f)) "#t\n")
```

Run tests

```
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

That's all for this post, folks! We added primitives with single arguments to the language. We upgraded our compiler infrastructure to easily add primitives. Unary primitives were especially easy to handle because we could just use `%eax` register for value of the argument. This won't be possible if there are more than one arguments. That'll be topic of the next post.

Working code at the end of this step can be found at my [Github repo](https://github.com/chsasank/scheme-incremental-compiler) with tag [`step_3_unary_primitives`](https://github.com/chsasank/scheme-incremental-compiler/releases/tag/step_3_unary_primitives)