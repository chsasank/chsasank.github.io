---
layout: post
title: "Intermediate Representations in Compilers: SSA is Functional Programming"
author: Sasank Chilamkurthy
twitter_image: "https://chsasank.com/assets/images/scheme_compiler/ssa.png"
---

In the [previous post](https://chsasank.com/compiler-intermediate-representations-1-ssa.html), we spoke about an intermediate representation (IR) of code called single static assignment (SSA) and how variables are never overwritten in SSA. We saw LLVM IR -- a very popular SSA IR for imperative languages -- for different constructs including functions, conditionals and loops. We have noted that SSA and another intermediate language popular for functional language called continuation passing style (CPS) are apparently similar. In this post, we will take one step towards making this clearer: we will observe how SSA form is actually functional programming!

I mean it shouldn't come as shocker: SSA form is all about single assignment to variables and one of the core tenet of functional programming is immutability of variables. We will closely follow the arguments of [Andrew Appel](https://www.cs.princeton.edu/~appel/)'s [paper](https://www.cs.princeton.edu/~appel/papers/ssafun.pdf). Nevertheless, we will follow the same approach of 'show by examples' from the previous post.

## SSA = Functional Programming

We'll reuse the examples from the previous post and transform the SSA to functional definition.


### Functions

Let's start with a ultra basic function:

```c
// ssa.c
int fun(int x, int y){
    return x + y;
}
```

LLVM IR:

```llvm
; ssa.ll
; function definition
define i32 @fun(i32 %x, i32 %y) {
    ; temporary variable is created
    ; addtmp = x + y
    %addtmp = add i32 %x, %y
    ; and that is returned
    ret i32 %addtmp
}
```

Now let's transform this to functional representation. This is very straight forward as we'll use simply use `let` to name intermediate variables.

```scheme
(define (fun x y)
  (let ((addtmp (+ x y)))
    addtmp))
```

### Variable Modification

Next let's see the how variable modification looks like:

```c
//ssa.c
int fun(int x, int y){
    x = (x * 5) / 3;
    y = y + 10;
    return x + y;
}
```

LLVM IR:

```llvm
; ssa.ll
define i32 @fun(i32 %x, i32 %y) {
    ; x1 = x * 5
    %x1 = mul nsw i32 %x, 5
    ; x2 = x1 / 3
    %x2 = sdiv i32 %x1, 3
    ; y1 = y + 10
    %y1 = add i32 %y, 10
    ; addtmp = x2 + y1
    %addtmp = add nsw i32 %x2, %y1
    ret i32 %addtmp
}
```

Again, this is pretty straight forward because we just have to use `let*`:

```scheme
(define (fun x y)
  (let* ((x1 (* x 5))
         (x2 (/ x1 3))
         (y1 (+ y 10))
         (addtmp (+ x2 y1)))
    addtmp))
```

### Conditionals

Now we're getting to interesting part! In SSA, we've had to introduce `phi` function to merge values from different conditional blocks. Let's recall the function we used in the previous post:

```c
//ssa.c
int fun(int x, int y){
    int z;
    if (x > 1) {
        z = x + y;
    }
    else if (x < -1) {
        z = - x + y;
    } else {
        z = x - y;
    }
    return z;
}
```

LLVM IR looked like this:

```llvm
; ssa.ll
define i32 @fun(i32 %x, i32 %y) {
    ; integer compare (icmp), signed greater than (sgt)
    ; x_gt_1 = (x > 1)
    %x_gt_1 = icmp sgt i32 %x, 1
    ; branch (br) if x_gt_1 (boolean/i1) to 'if_gt' 
    ; otherwise goto 'else'
    br i1 %x_gt_1, label %if_gt, label %else

if_gt:
    ; add = x + y
    %add = add i32 %x, %y
    ; unconditional jump to 'end'
    br label %end

else:
    ; integer compare (icmp), signed less than (slt)
    ; x_lt_neg1 = (x < -1)
    %x_lt_neg1 = icmp slt i32 %x, -1
    ; branch to 'if_lt' or 'if_else' conditional on x_lt_neg1
    br i1 %x_lt_neg1, label %if_lt, label %if_else

if_lt:
    ; sub1 = 0 - x
    %sub1 = sub i32 0, %x
    ; add1 = sub1 + y
    %add1 = add i32 %sub1, %y
    ; unconditional jump to 'end'
    br label %end

if_else:
    ; sub2 = x - y
    %sub2 = sub i32 %x, %y
    ; unconditional jump to 'end'
    br label %end

end:
    ; phi instruction: list of [variable, label]
    %z = phi i32 [ %add, %if_gt ], [ %add1, %if_lt ],
                    [ %sub2, %if_else ]
    ret i32 %z
}
```

We'll use lambda as the [ultimate goto](https://www2.cs.sfu.ca/CourseCentral/383/havens/pubs/lambda-the-ultimate-goto.pdf) to translate branch instructions into lambdas. Each label above will be transformed into a procedure. Let's take a look:


```scheme
(define (fun x y)
    ; each label is a lambda
    (define (if-gt)
        (let ((add (+ x y)))
            (end add)))

    (define (else)
        (let ((x-lt-neg1 (< x -1)))
            (if x-lt-neg1 (if-lt) (if-else))))

    (define (if-lt)
        (let* ((sub1 (- 0 x))
               (add1 (sub1 y)))
            (end add1)))

    (define (if-else)
        (let ((sub2 (- x y)))
            (end sub2)))

    (define (end z) z)
    
    (let ((x-gt-1 (> x 1)))
        (if x-gt-1 (if-gt) (else))))
```

Read this code along with above LLVM IR and you can see how I transformed the code. Of specific interest is this line in LLVM

```
%z = phi i32 [ %add, %if_gt ], [ %add1, %if_lt ],
                    [ %sub2, %if_else ]
```

Observe the arguments to `end` in my *scheme* transformation:
1. In `if-gt` procedure, `z` is `add`
2. In `if-lt` procedure, `z` is `add1`
3. In `if-else` procedure, `z` is `sub2`

This is exactly what `phi` instruction says in above line!

This should make the correspondence of `phi` instruction in LLVM IR to our functional translation of it. `phi` specifies actual parameter for formal parameter when called in a specific control flow block. In other words, `phi` got translated into arguments for our label procedures.

So far this is the algorithm we discovered to transform SSA to functional program:

1. Use `let` for intermediate variables
2. Labels get translated to procedures using `define`
3. `phi` instructions specify formal and actual parameters to some of these procedures

### Loops

Now that we got a hang of this, let's try our hand at the GCD program from the last post. Recall the C program:

```c
int fun(int x, int y){
    while (x != y) {
        if (x > y)
            x = x - y;
        else
            y = y - x;
    }
    return x;
}
```

And its LLVM IR

```llvm
define i32 @fun(i32 %x, i32 %y) {
    br label %loop

loop:
    ; x1 = x or new_x
    %x1 = phi i32 [ %x, %entry ], [ %new_x, %if_x_greater ]
    ; y1 = y or new_y
    %y1 = phi i32 [ %y, %entry ], [ %new_y, %if_y_greater ]
    ; integer compare (icmp) not equal (ne)
    ; cmp = (x1 != y1)
    %cmp = icmp ne i32 %x1, %y1
    ; jump conditioal on %cmp to 'if_cond' or 'end' 
    br i1 %cmp, label %if_cond, label %end

if_cond:
    ; integer compare (icmp) signed greater than (sgt)
    ; x_gt_y = (x1 > y1)
    %x_gt_y = icmp sgt i32 %x1, %y1
    ; jump conditioal on %x_gt_y to 'if_x_greater' or 'if_y_greater' 
    br i1 %x_gt_y, label %if_x_greater, label %if_y_greater

if_x_greater:
    ; new_x = (x1 - y1)
    %new_x = sub i32 %x1, %y1
    ; unconditional jump to the top of loop
    br label %loop

if_y_greater:
    ; new_y = (y1 - x1)
    %new_y = sub i32 %y1, %x1
    ; unconditional jump to the top of loop
    br label %loop

end:
    ; return x1
    ret i32 %x1
}
```

Here's how scheme translation of this would look like:

```scheme
(define (fun x y)
    (define (loop x1 y1)
        (define (if-cond)
            (let ((x-gt-y (> x1 y1)))
                (if x-gt-y (if-x-greater) (if-y-greater))))

        (define (if-x-greater)
            (let ((new-x (- x1 y1)))
                (loop new-x y1)))

        (define (if-y-greater)
            (let ((new-x (- y1 x1)))
                (loop x1 new-y))))

        (define (end) x1)
        
        (let ((cmp (not (= x1 y1))))
            (if cmp (if-cond) (end)))
    
    (loop x y))
```

Note the nesting of the procedures - we did this so that we don't have to explicitly pass `x1` and `y1` to `if-cond`, `if-x-greater` and `if-y-greater`. That is, we used the nested scope idea of scheme. In SSA form, this nesting semantics is implicit while it is explicit in our functional program. In fact the algorithm for optimal nesting is also same as the optimal placement of phi functions [1]. We will discuss this further in a future post.

## References:

1. Andrew W. Appel, [SSA is Functional Programming](https://www.cs.princeton.edu/~appel/papers/ssafun.pdf), 1998
2. Adrian Simpson, [Global Analysis & SSA](https://www.cs.cornell.edu/courses/cs6120/2020fa/lesson/5/), 2020