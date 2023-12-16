---
layout: post
title: "Scheme Compiler in Incremental Steps: Immediate Constants"
author: Sasank Chilamkurthy
twitter_image: "https://1010jms.github.io/images/scheme.png"
---

In the previous part, we compiled integers and created linker and runtime. In this part, we'll add few more language features to our compiler

## Immediate Constants

In scheme, we have values other than fixnum integers. We have immediate values that can be stored directly in a machine word: Booleans, characters and the empty list. We can not use fixnums to denote booleans or characters because they are disjoin types. These types must also be available at runtime to print them nicely.

We will use a technique called "tagged pointer representation" to encode these values. We will dedicate a few lower bits of the machine word for type information and rest for storing the value. Every type of value is defined by a mask and a tag. We will follow the following conventions:

1. Fixnums: Lower two bits must be `00`. Rest of the 30 bits hold the value of a fixnum.
2. Characters: Lower 8 bits must be `00001111` leaving 24 bits for value. We'll actually use 7 of these for encoding ASCII characters.
3. Booleans: Lower 7 bits must be `0011111` and 1 bit will be used for boolean
4. Empty list: Given value `00101111`

We need to extend our compiler `compile-program` to handle immediate types appropriately. We will use functions `logior` and `ash` from guile for bitwise or and bitwise shift respectively.

```scheme
;compiler.scm
(define fixnum-shift 2)
(define char-shift 8)
(define char-tag #b00001111)
(define bool-shift 7)
(define bool-tag #b0011111)
(define null-val #b00101111)

(define (immediate? x)
    ;check if x is one of immediate type
    (or (integer? x) (char? x) (boolean? x) (null? x)))

(define (immediate-rep x)
    ;Convert x into tagged pointer representation
    (cond
        ; bit shift integers by 2 bits
        ((integer? x) (ash x fixnum-shift))
        ; bit shift by 8 integers and add tag
        ; for character
        ((char? x)
            (logior char-tag (ash (char->integer x) char-shift)))
        ; same for boolean
        ((boolean? x) 
            (logior bool-tag (ash (if x 1 0) bool-shift)))
        ; empty list
        ((null? x) null-val)
        (else (error "no immediate representation for" x))))

(define (compile-program x)
    (emit "movl $~a, %eax" (immediate-rep x))
    (emit "ret"))
```

We also need to modify runtime so that we can print the right string representation of the tagged values.

```c
//runtime.c
#include <stdio.h>

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

int main(int argc, char** argv){
    int val = scheme_entry();
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
    return 0;
}
```

Finally add tests to `tests.scm` check if everything works fine

```
; tests.scm
; ....
; keep above lines as is and add following lines
(run-test 536870911 "536870911\n")
(run-test -536870912 "-536870912\n")

; characters
(run-test #\! "#\\!\n")
(run-test #\" "#\\\"\n")
(run-test #\# "#\\#\n")
(run-test #\' "#\\'\n")

(run-test #\0 "#\\0\n")
(run-test #\1 "#\\1\n")
(run-test #\; "#\\;\n")

(run-test #\A "#\\A\n")
(run-test #\B "#\\B\n")
(run-test #\Y "#\\Y\n")
(run-test #\Z "#\\Z\n")
(run-test #\a "#\\a\n")
(run-test #\b "#\\b\n")
(run-test #\y "#\\y\n")
(run-test #\z "#\\z\n")

; booleans
(run-test #t "#t\n")
(run-test #f "#f\n")

; empty list
(run-test '() "()\n")
```

Run tests using

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
!: passed
": passed
#: passed
': passed
0: passed
1: passed
;: passed
A: passed
B: passed
Y: passed
Z: passed
a: passed
b: passed
y: passed
z: passed
#t: passed
#f: passed
(): passed
```

That's all for this post. In the next post, we will add support for primitives with single arguments.