---
layout: post
title: "Scheme Compiler in Incremental Steps: Local Variables"
author: Sasank Chilamkurthy
twitter_image: "https://1010jms.github.io/images/scheme.png"
---

In the previous post, we added stack to our implementation of scheme. This allowed us to store intermediate results and therefore binary primitives like `(+ a b)`. Since we now have stack, adding local variables should be straightforward. We will implement two new syntactic forms: `let` and variable references.

For reference, this is how `let` works in scheme:

```scheme
(let 
    ; variable bindings
    ((x 2) (y (+ 1 3)))
    ; body
    (+ x y))
```

Variables bound by `let` become visible only in the body of `let`. They are not visible in right hand expression of variable bindings. For example following is illegal:

```scheme
; illegal because binding of y doesn't know about x
(let ((x 2) (y (+ x 1)))
    (+ x y))
```

## Infrastructure Upgrade: Environment

In order to implement `let`, we need to maintain an environment that maintains mapping from local variables to location where they are stored. Since we store values in the stack, our environment maps every variable to its stack index. Note that this environment is a compile time construct just like stack index (`si`) variable introduced in last post. We'll represent our environment with [association list](https://www.gnu.org/software/guile/manual/guile.html#Association-Lists) or alist for short.

Let's initialize our environment with empty list and pass it around to `emit-expr` and wherever `emit-expr` is used. We will also do small refactor by introducing two convenience functions for saving to stack and computing next stack index.

```scheme
; edit in compiler.scm
(define (compile-program x)
    ; initialize stack index - word-size so as not to 
    ; overwrite return address
    ; -> env starts empty
    (emit-expr (- word-size) '() x)
    (emit "ret"))

; -> add env to emit-expr arguments
(define (emit-expr si env expr)
    (cond
        ((immediate? expr) (emit-immediate expr))
        ((primcall? expr) (emit-primcall si env expr))
        (else (error "syntax error"))))

(define-syntax define-primitive
    (syntax-rules ()
        ; -> note env after si
        ((_ (prim-name si env arg* ...) b b* ...)
            (begin
                (set-symbol-property! 'prim-name '*is-prim* #t)
                (set-symbol-property! 'prim-name '*arg-count* 
                    (length '(arg* ...)))
                ; -> emitters now take in env as param
                (set-symbol-property! 'prim-name '*emitter*
                    (lambda (si env arg* ...) b b* ...))))))

(define (emit-primcall si env expr)
    (let ((prim (car expr))
          (args (cdr expr)))
        ; add env as arg to emitters
        (apply (primitive-emitter prim) si env args)))

; convenience functions
(define (next-stack-index si)
    (- si word-size))

(define (emit-stack-save si)
    ; save output of previous instructions to stack
    (emit "movl %eax, ~a(%rsp)" si))

; add env to define-primitive
(define-primitive (+ si env arg1 arg2)
    ; add env here
    (emit-expr si env arg1)
    
    ; save result of arg1 in stack
    (emit-stack-save si)

    ; move stack index by a word so that 
    ; above is not overwritten
    ; add env here too
    (emit-expr (next-stack-index si) env arg2)

    ; add result from stack with arg2 result
    ; this works because of integer tag = b00
    (emit "addl ~a(%rsp), %eax" si))

; repeat for all primitives
```

Once `env` is tracked across all the code, run tests to ensure our infrastructure works.

```bash
$ guile tests.scm
...
(>= 16 (+ 13 13)): passed
(>= (+ 13 1) 16): passed
(>= (+ 13 3) 16): passed
(>= (+ 13 13) 16): passed
```

## Add Variables and `let`

Let's now starting referencing variables from env and bind them using let. For variable reference, we just have to retrieve stack pointer of the relevant variable from `env` and move it to `%eax`.

```scheme
; in compiler.scm
(define (variable? x)
    ; variable is any symbol that's not a primitive
    (and (symbol? x) (not (primitive? x))))

(define (emit-variable-ref env var)
    (cond
        ((assoc var env)
            (emit "movl ~a(%rsp), %eax" (cdr (assoc var env))))
        (else (error "unknown variable: " var))))
```

Now let's write pattern matcher for `let` and its emitter:

```scheme
; in compiler.scm
(define (let? expr)
    (and (pair? expr) (eq? (car expr) 'let)))

(define (let-bindings expr)
    ; get bindings list from expr
    (car (cdr expr)))

(define (let-body expr)
    ; get bindings list from expr
    (car (cdr (cdr expr))))

(define (process-let si env bindings body)
    ; recursively add bindings to env
    (cond
        ((null? bindings)
            (emit-expr si env body))
        (else
            (let* ((b (car bindings))
                    (lhs (car b))
                    (rhs (car (cdr b))))
                ; emit code for rhs of binding b
                (emit-expr si env rhs)
                ; save output into stack
                (emit-stack-save si)
                (process-let
                    (next-stack-index si) (acons lhs si env)
                    (cdr bindings) body)))))

(define (emit-let si env expr)
    (process-let si env (let-bindings expr) (let-body expr)))
```

Let's put it all together in `compile-program`:

```scheme
; edit in compiler.scm
(define (compile-program x)
    ; initialize stack index - word-size so as not to 
    ; overwrite return address
    (emit-expr (- word-size) '() x)
    (emit "ret"))

(define (emit-expr si env expr)
    (cond
        ((immediate? expr) (emit-immediate expr))
        ((variable? expr) (emit-variable-ref env expr))
        ((let? expr) (emit-let si env expr))
        ((primcall? expr) (emit-primcall si env expr))
        (else (error "syntax error"))))
```

Add following tests for `let`:

```scheme
; in tests.scm
(run-test '(let ((x 5)) x) "5\n")
(run-test '(let ((x (+ 1 2))) x) "3\n")
(run-test '(let ((x (+ 1 2))) 
     (let ((y (+ 3 4)))
       (+ x y))) 
   "10\n")
(run-test '(let ((x (+ 1 2))) 
     (let ((y (+ 3 4)))
       (- y x)))
   "4\n")
(run-test '(let ((x (+ 1 2))
         (y (+ 3 4)))
     (- y x))
   "4\n")
(run-test '(let ((x (let ((y (+ 1 2))) (* y y))))
     (+ x x))
   "18\n")
(run-test '(let ((x (+ 1 2)))
     (let ((x (+ 3 4)))
       x)) 
   "7\n")
(run-test '(let ((x (+ 1 2)))
     (let ((x (+ x 4)))
       x)) 
   "7\n")
(run-test '(let ((t (let ((t (let ((t 
    (let ((t (+ 1 2))) t))) t))) t))) t)
   "3\n")
(run-test '(let ((x 12))
     (let ((x (+ x x)))
       (let ((x (+ x x)))
         (let ((x (+ x x)))
           (+ x x)))))
   "192\n")
```

Run the tests using `guile tests.scm` and verify that all the tests pass.

That's all for this post folks! We added local variables to our compiler/language by adding compile time `env` variable. Values are actually stored on stack during runtime.

Working code at the end of this step can be found at my [Github repo](https://github.com/chsasank/scheme-incremental-compiler) with tag [`step_4_binary_primitives`](https://github.com/chsasank/scheme-incremental-compiler/releases/tag/step_4_binary_primitives)
