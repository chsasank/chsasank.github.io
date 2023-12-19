---
layout: post
title: "Scheme Compiler in Incremental Steps: Heap Allocation"
author: Sasank Chilamkurthy
twitter_image: "https://1010jms.github.io/images/scheme.png"
---

A lisp is not a lisp if it doesn't support lists! In this post, let's implement complicated data structure like lists, strings, etc. These data structures don't fit in single word like they did for integers, characters and others from our [earlier tutorial](https://chsasank.com/scheme-compiler-2-constants.html). So we will preallocate yet another contiguous area of memory called heap. Our complex data structures will be stored in this heap.

We will use the same tagged pointer representation [we used](http://localhost:4000/scheme-compiler-2-constants.html) for integers and characters for our objects. We will use the following 3 bit tags:

* `001`: pairs
* `010`: vectors
* `011`: strings
* `101`: symbols
* `110`: closures
* `000`, `100`: already used by integers
* `111`: already used by chars, booleans and empty list.

This means only rest of the bits can be used for addressing/pointing to the data. We'll ensure that the lower 3 bits of our pointers are always `000` - i.e. we will allocate objects on heap only at 8 byte boundaries. This way we can recover the actual address from tagged address by simply subtracting the tag value. Our word size is anyway 8 bytes so we don't need to do any extra book keeping!

![Heap](/assets/images/scheme_compiler/heap.png)

## Infrastructure Upgrade: Heap

Just like how [we did](https://chsasank.com/scheme-compiler-4-binary-primitives.html) for stack, we will create a heap in `run_time.c` using `allocate_protected_space`. We will pass the pointer to the beginning of the heap to `scheme_entry`. In `scheme_entry`, we will dedicate a register `%rsi` to hold the allocation pointer. Every time an object is constructed, the value of `%rsi` is incremented according to the size of the object. Note that our heap grows from low to high addresses as opposed to stack.

```c
// edit in runtime.c
int main(int argc, char** argv){
    int stack_size = (16 * 4096);   /* holds 16k cells*/
    char *stack_top = allocate_protected_space(stack_size);
    char *stack_base = stack_top + stack_size;

    int heap_size = (160 * 4096);  /* holds 160k cells */
    char *heap_top = allocate_protected_space(heap_size);
    
    int val = scheme_entry(stack_base, heap_top);
    print_val(val);

    deallocate_protected_space(stack_top, stack_size);
    deallocate_protected_space(heap_top, heap_size);
    return 0;
}
```

According to [Linux x86-64 calling convention](https://wiki.cdot.senecacollege.ca/wiki/X86_64_Register_and_Instruction_Quick_Start), `%rsi` holds the second argument passed to a function. In our case, second argument is address to `heap_top` which is being stored in `%rsi`. This is exactly as we desired. Therefore, we don't have to modify any of the linker assembly code.

## Add Pairs

In lisp, pairs form the basis for lists. Pair is composed of `car` and `cdr`. Primitive `cons` creates a pair like this: `(cons car cdr)`. A list is formed by storing value in `car` and pointer to next pair in `cdr` until we hit empty list `'()` in `cdr`. Recall that empty list `'()` is an immediate constant we defined in [previous step](https://chsasank.com/scheme-compiler-2-constants.html). In other words:

```scheme
(cons 1 (cons 2 (cons 3 (cons 4 '()))))
=> (1 2 3 4)
```

![List](/assets/images/scheme_compiler/list.png)

We will also define primitives `pair?` as a predicate for pairs. Primitives `car` and `cdr` take a pair as argument and return parts of the pair. Let's get into implementation:


```scheme
; lists
(define-primitive (pair? si env expr)
    (emit-expr si env expr)
    ; apply heap mask (1 bits heap-shift times)
    ; to lower byte of %eax
    (emit "and $~s, %al" (- (ash 1 heap-shift) 1))
    (emit "cmp $~s, %al" pair-tag)
    (emit-zeroflag-to-bool))

(define-primitive (cons si env arg1 arg2)
    ; compute car
    (emit-expr si env arg1)
    ; store it in stack
    (emit-stack-save si)
    ; compute cdr
    ; increment stack index so that car is not overwritten
    (emit-expr (next-stack-index si) env arg2)

    ; save cdr in next heap word
    (emit "mov %rax, ~a(%rsi)" word-size)

    ; save car to start of heap by saving previous 
    ; result to scratch first because we can't move
    ; address to address direclty.
    (emit "mov ~a(%rsp), %rax" si)
    (emit "mov %rax, 0(%rsi)")

    ; move address to result
    (emit "mov %rsi, %rax")
    ; add pair tag
    (emit "or $~a, %rax" pair-tag)
    ; increment heap pointer
    (emit "add $~a, %rsi" (* 2 word-size)))

(define-primitive (car si env arg1)
    (emit-expr si env arg1)
    (emit "mov ~a(%rax), %rax" (- pair-tag)))

(define-primitive (cdr si env arg1)
    (emit-expr si env arg1)
    (emit "mov ~a(%rax), %rax" (- word-size pair-tag)))
```

Let's add tests:

```
(run-test '(pair? (cons 1 2)) "#t\n")
(run-test '(car (cons 1 2)) "1\n")
(run-test '(cdr (cons 1 2)) "2\n")
(run-test '(car (cons 1 (cons 2 3))) "1\n")
(run-test '(car (cdr (cons 1 (cons 2 3)))) "2\n")
(run-test '(pair? (cons 1 2)) "#t\n")
(run-test '(pair? 12) "#f\n")
(run-test '(pair? #t) "#f\n")
(run-test '(pair? #f) "#f\n")
(run-test '(pair? ()) "#f\n")
(run-test '(integer? (cons 12 43)) "#f\n")
(run-test '(boolean? (cons 12 43)) "#f\n")
(run-test '(null? (cons 12 43)) "#f\n")
(run-test '(not (cons 12 43)) "#f\n")
(run-test '(if (cons 12 43) 32 43) "32\n")
(run-test '(car (cons 1 23)) "1\n")
(run-test '(cdr (cons 43 123)) "123\n")
(run-test '(car (car (cons (cons 12 3) (cons #t #f)))) "12\n")
(run-test '(cdr (car (cons (cons 12 3) (cons #t #f)))) "3\n")
(run-test '(car (cdr (cons (cons 12 3) (cons #t #f)))) "#t\n")
(run-test '(cdr (cdr (cons (cons 12 3) (cons #t #f)))) "#f\n")
(run-test '(let ([x (let ([y (+ 1 2)]) (* y y))])
            (cdr (cons x (+ x x)))) "18\n")
```

And verify everything works:

```
$ guile tests.scm
...
(null? (cons 12 43)): passed
(not (cons 12 43)): passed
(if (cons 12 43) 32 43): passed
(car (cons 1 23)): passed
(cdr (cons 43 123)): passed
(car (car (cons (cons 12 3) (cons #t #f)))): passed
(cdr (car (cons (cons 12 3) (cons #t #f)))): passed
(car (cdr (cons (cons 12 3) (cons #t #f)))): passed
(cdr (cdr (cons (cons 12 3) (cons #t #f)))): passed
(let ((x (let ((y (+ 1 2))) (* y y)))) (cdr (cons x (+ x x)))): passed
```

That's all for this post folks. We added heap to our compiler and allocated pairs in it. We saw how these pairs can be used to construct lists. However, we didn't add pretty printing lists because that's complicated. Nor did we add to our simple declaration of lists like `(x y z)` to our frontend. A more fully featured compiler would implement those.