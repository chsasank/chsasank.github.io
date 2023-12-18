---
layout: post
title: "Scheme Compiler in Incremental Steps: Conditional Expressions"
author: Sasank Chilamkurthy
twitter_image: "https://1010jms.github.io/images/scheme.png"
---

In this post, let's implement conditional expressions: `(if test conseq altern)`. This is simple to implement at assembly level. We need to employ two labels to transfer control to `conseq` or `altern` based on whether `test` is true. In scheme all values except `#f` are considered truthy - that includes empty list `'()`.

We'll create a function to create a unique label every time it's called:

```scheme
; add in compiler.scm
(define label-count 0)

(define (unique-label)
    (let ((L (format #f "L_~a" label-count)))
        (set! label-count (+ label-count 1))
        L))
```

Let's now a pattern matcher and emitter for `if`:

```scheme
; add in compiler.scm
(define (if? expr)
    (and (pair? expr) (eq? (car expr) 'if)))

(define (emit-if si env expr)
    (let ((altern-label (unique-label))
          (end-label (unique-label))
          (test (cadr expr))
          (conseq (caddr expr))
          (altern (cadddr expr)))
        (emit-expr si env test)
        (emit "cmpl $~a, %eax" (immediate-rep #f))
        ; jump to alternative label if test is 
        ; equal to false
        (emit "je ~a" altern-label)
        (emit-expr si env conseq)
        ; jump unconditionally to end so that altern 
        ; is not executed if test is truthy
        (emit "jmp ~a" end-label)
        ; emit label for altern
        (emit "~a:" altern-label)
        (emit-expr si env altern)
        ; emit label for end
        (emit "~a:" end-label)))
```

Let's add tests:

```scheme
; in tests.scm
; if
(run-test '(if #t 12 13) "12\n")
(run-test '(if #f 12 13) "13\n")
(run-test '(if 0 12 13)  "12\n")
(run-test '(if () 43 ()) "43\n")
(run-test '(if #t (if 12 13 4) 17) "13\n")
(run-test '(if #f 12 (if #f 13 4)) "4\n")
(run-test '(if #\X (if 1 2 3) (if 4 5 6)) "2\n")
(run-test '(if (not (boolean? #t)) 15 (boolean? #f)) "#t\n")
(run-test '(if (if (char? #\a)
                  (boolean? #\b) (integer? #\c)) 119 -23) "-23\n")
(run-test '(if (if (if (not 1) (not 2) (not 3)) 4 5) 6 7) "6\n") 
(run-test '(if (not (if 
                (if (not 1) (not 2) (not 3)) 4 5)) 6 7) "7\n") 
(run-test '(not (if (not (if 
                (if (not 1) (not 2) (not 3)) 4 5)) 6 7)) "#f\n") 
(run-test '(if (char? 12) 13 14) "14\n")
(run-test '(if (char? #\a) 13 14) "13\n")
(run-test '(add1 (if (sub1 1) (sub1 13) 14)) "13\n")
```

Run tests and verify everything works:

```bash
$ guile tests.scm
...
(if () 43 ()): passed
(if #t (if 12 13 4) 17): passed
(if #f 12 (if #f 13 4)): passed
(if X (if 1 2 3) (if 4 5 6)): passed
(if (not (boolean? #t)) 15 (boolean? #f)): passed
(if (if (char? a) (boolean? b) (integer? c)) 119 -23): passed
(if (if (if (not 1) (not 2) (not 3)) 4 5) 6 7): passed
(if (not (if (if (not 1) (not 2) (not 3)) 4 5)) 6 7): passed
(not (if (not (if (if (not 1) (not 2) (not 3)) 4 5)) 6 7)): passed
(if (char? 12) 13 14): passed
(if (char? a) 13 14): passed
(add1 (if (sub1 1) (sub1 13) 14)): passed
```

That's all for this post, folks! This is one of the easiest steps because our assembly supports conditional jump.

Working code at the end of this step can be found at my [Github repo](https://github.com/chsasank/scheme-incremental-compiler) with tag [`step_6_conditional_expressions`](https://github.com/chsasank/scheme-incremental-compiler/releases/tag/step_6_conditional_expressions).
