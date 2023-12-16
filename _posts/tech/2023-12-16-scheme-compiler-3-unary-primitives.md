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

## Adding primitives

We're finally ready to add primitives `add1` and `subl1`. We will use `addl`/`subl` machine instructions

```scheme
; in compiler.scm
(define-primitive (add1 arg)
    (emit-expr arg)
    (emit "addl $~a, %eax" (immediate-rep 1)))
```

Add the following tests to `tests.scm` to verify everything works

```scheme
; add to tests.scm
(run-test '(add1 0) "1\n")
(run-test '(add1 -1) "0\n")
(run-test '(add1 1) "2\n")
(run-test '(add1 -100) "-99\n")
(run-test '(add1 536870910) "536870911\n")
(run-test '(add1 -536870912) "-536870911\n")
(run-test '(add1 (add1 0)) "2\n")
(run-test '(add1 (add1 (add1 (add1 (add1 (add1 12)))))) "18\n")
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
```