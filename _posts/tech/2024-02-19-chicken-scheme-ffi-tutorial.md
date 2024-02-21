---
layout: post
title: "Foreign Function Interfaces, or How to Expose C Functions in Scheme"
author: Sasank Chilamkurthy
twitter_image: 
---

One of my [biggest hit posts](https://chsasank.com/lua-c-wrapping.html) is how to call C functions from Lua. This feature is called as foreign function interface or FFI in short. In this post, I am going to show how to do FFI in Scheme. I will also add some wisdom to the mix :).

## FFI and Deep Learning

 Lua language is especially designed for FFI: so much so that [original Torch](http://torch.ch/)'s frontend is in Lua. Torch team then moved the frontend to Python because its FFI is pretty good as well and the FFI performance downgrade is negligible compared to the time taken by CUDA code. This migration to Python has led PyTorch to reach the [current heights](https://www.assemblyai.com/blog/pytorch-vs-tensorflow-in-2023/) and now it controls the ecosystem!

FFI allowed the torch maintainers to write a domain specific language for deep learning. The distinction between a library and a language is more blurry than it appears to be. Users of racket language will immediately recognize [this](https://www2.ccs.neu.edu/racket/pubs/pldi11-thacff.pdf). Besides learning PyTorch indeed feels like learning a new language. In fact, PyTorch syntax draws its legacy from an old school language called [APL](https://en.wikipedia.org/wiki/APL_(programming_language)).

Deep down, both PyTorch and LuaTorch are interpreters. What is an interpreter and how is it different from a compiler? Interpreter executes a language line by line without bothering about overall efficiency while compiler looks at the whole program and generates optimal code. In other words, compilers listen to you fully before translating it while interpreters just convert sentence by sentence. Python is an interpreted language while C is a compiled language.

Foreign function interfaces allowed Torch to interpret over the CUDA code without optimizing everything together. I should add a word that this is changing with PyTorch 2.0 and `torch.compile`.

## Chicken Scheme

That's a quick history lesson and should motivate you to learn more about FFI. My current favorite language is lisp/scheme. Lisp is so simple that you can write an [interpreter](https://norvig.com/lispy.html) in python in about [150 lines](https://norvig.com/lis.py). Lisp/Scheme is a family of languages and has multiple flavours - I like chicken and guile (yep, they [are](https://call-cc.org/) [real](https://www.gnu.org/software/guile/)!). In this post, let's focus on chicken scheme because it can both be interpreted and actually compiled to C. This means that FFI with C is especially pain free.

Let's start with installing chicken scheme. I am going to build from sources because it's easy!

```bash
wget https://code.call-cc.org/releases/5.3.0/chicken-5.3.0.tar.gz
tar -xvf chicken-5.3.0.tar.gz
cd chicken-5.3.0
make -j
sudo make install
```

This should install chicken scheme interpreter (csi) and compiler (csc):

```bash
$ which csi
/usr/local/bin/csi
$ which csc
/usr/local/bin/csc
```

Let's see how chicken scheme works by writing a hello world

```
$ csi
CHICKEN
(c) 2008-2021, The CHICKEN Team
(c) 2000-2007, Felix L. Winkelmann
Version 5.3.0 (rev e31bbee5)
linux-unix-gnu-x86-64 [ 64bit dload ptables ]

Type ,? for help.
#;1> (display "hello world\n")
hello world
#;2>
```

Ok how about compiling? Let's create a file `hello.s` and compile it:

```
$ cat << EOF > hello.s
(display "hello world\n")                  
EOF
$ csc hello.s
$ ls
hello  hello.s
$ ./hello
hello world
```

You can also observe the actual generated C code:

```
$ csc hello.s -to-stdout | head
/* Generated from hello.s by the CHICKEN compiler
   http://www.call-cc.org
   Version 5.3.0 (rev e31bbee5)
   linux-unix-gnu-x86-64 [ 64bit dload ptables ]
   command line: hello.s -to-stdout -to-stdout
   uses: eval library
*/
#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
```

## Chicken FFI

Now that we've seen how to use chicken scheme, let's make it talk with a simple C library. We will use low level FFI API using chicken's [`foreign`](http://wiki.call-cc.org/man/5/Module%20(chicken%20foreign)) module but this option requires us to write a bit of [glue code](https://wiki.call-cc.org/man/5/Getting%20started#accessing-c-libraries-). There are other high level options like [`bind`](http://wiki.call-cc.org/eggref/5/bind) and [`lazy-ffi`](https://wiki.call-cc.org/eggref/5/lazy-ffi) but let's go low level because it is not particularly hard. Besides `bind` uses a parser for restricted subset of C/C++ to automatically generate these wrappers. This parser is incomplete and it might fail.

Here's a very simple C file `fib.c` to calculate fibonacci numbers

```c
/* fib.c */
#include <math.h>

int fib(int n) {
  int prev = 0, curr = 1;
  int next; 
  int i; 
  for (i = 0; i < n; i++) {
    next = prev + curr;
    prev = curr;
    curr = next; 
  }``
  return curr;
}
```

Now, let's use this function in scheme `fib-user.scm`. So we first need to import chicken library called `foreign` to get access to `foreign-lambda`. Then we include actual C code between `#>` and `<#`. In our case, it'll be `extern` function declaration and some other example code. Finally, we expose that function to scheme using `foreign-lambda`.

```scheme
; fib-user.s

(import (chicken foreign))
; insert actual C code
#>
  #include <math.h>
  extern int fib(int n);
  int lshift(int x, int y){
    return x << y;
  }
<#
(define xfib (foreign-lambda int "fib" int))
z(define xsin (foreign-lambda double "sin" double))
(define xcos (foreign-lambda double "cos" double))
(define xlshift (foreign-lambda int "lshift" int int))

(print "fib(10) = " (xfib 10))
(print "sin(0) = "(xsin 0))
(print "cos(0) = "(xcos 0))
(print "lshift(3, 2) = "(xlshift 3 2))
```

Compile everything and run the fib-user:

```bash
$ csc fib-user.s fib.c
$ ./fib-user
fib(10) = 89
sin(0) = 0.0
cos(0) = 1.0
lshift(3, 2) = 12
```

As you can see, writing bindings is a fairly repetitive process and same data is presented in the following places:
1. Function declaration in `fib.c`
2. `extern` in `fib-user.s`
3. `foreign-lambda` in `fib-user.s`

This is the price we have to pay to create bridges between two languages. Modules like [`bind`](http://wiki.call-cc.org/eggref/5/bind) automate this process by using macros. But problem with modules like these is that they need to parse the original C code and then generate the above code. A full fledged C parser is usually out of scope for these modules.

References:
1. [Chicken getting started](http://wiki.call-cc.org/man/5/Getting%20started)
2. [Chicken manual for interface with external functions and variables](http://wiki.call-cc.org/man/5/Interface%20to%20external%20functions%20and%20variables)
3. [Chicken-Scheme FFI Examples](https://www.accidentalrebel.com/chicken-scheme-ffi-examples.html)
4. [Chicken documentation for module (chicken foreign)](http://wiki.call-cc.org/man/5/Module%20(chicken%20foreign))
