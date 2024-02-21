---
layout: post
title: "How Common Lisp Met C++"
author: Sasank Chilamkurthy
twitter_image: 
---

I have tried guile and [chicken scheme](https://chsasank.com/chicken-scheme-ffi-tutorial.html) and I actually hated the FFI experience for the lack of documentation. I am not writing them off yet - Chicken still is so small and nice. But I have decided to try something new: Clasp. [Clasp](https://clasp-developers.github.io/) is a LLVM based common lisp implementation. While not as performant as SBCL, it's still pretty damn good. The killer feature is its C++ interoperability.

[Dr. Meister](https://drmeister.wordpress.com/about/) developed clasp because he wanted lisp but he need it to interface with C++ libraries. So he went around and implemented the whole common lisp in LLVM. By the way, he's your proverbial 'mad scientist' who is using clasp to design molecules. There's a nice [youtube video](https://www.youtube.com/watch?v=8X69_42Mj-g) where he explains motivation and architecture of clasp. Highly recommend.

<figure>
<label for="mn-fig-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-1" class="margin-toggle">
<span class="marginnote">Common Lisp Standard</span>
<img src="https://m.media-amazon.com/images/I/81mhPQ6C-yL._AC_UF1000,1000_QL80_.jpg" alt="Common Lisp Standard">
</figure>

## Building clasp

Anyway let's start with installing clasp. We will [build from scratch](https://github.com/clasp-developers/clasp/wiki/Building-and-Installing-from-Source) because we're hardcore (and also because we need for adding extensions). I recommend using distrobox to try out all this stuff and manage dependencies easily. See my [earlier tutorial](https://chsasank.com/intel-arc-gpu-driver-oneapi-installation.html) on how to install it.

```bash
distrobox create --name clasp-reactor --image ubuntu:22.04
distrobox enter clasp-reactor
```

We will start with installing dependencies

```bash
sudo apt update
sudo apt install -y binutils-gold clang-15 libclang-15-dev libfmt-dev \
    libboost-dev libclang-cpp15-dev libelf-dev libgmp-dev libunwind-dev \
    llvm-15 ninja-build sbcl pkg-config git
```

Next clone clasp and [build](https://github.com/clasp-developers/clasp/wiki/Building-and-Installing-from-Source) it

```bash
git clone https://github.com/clasp-developers/clasp.git
cd clasp
./koga
ninja -C build
```

This will kick start an interesting process:
1. `iclasp`: initial version of clasp, written in c++. It uses a virtual machine sort of like JVM
2. `cclasp`: Uses `iclasp` to compile standard library and [cleavir](https://github.com/s-expressionists/Cleavir) based compiler.

You'll see that the first step is normal C++ build but the second one will be compiling lisp files

```
First step:
...
[366/538] Compiling ../src/core/bytecode_compiler.cc 
...

Second step:
...
; Compiling file: SYS:SRC;LISP;KERNEL;LSP;PROLOGUE.LISP
; Writing FASO file to: SYS:LIB;SRC;LISP;KERNEL;LSP;PROLOGUE.FASO
..
```

`cclasp` is the one that we will use as `clasp`. It is a full common lisp implementation. Verify everything works by invoking clasp

```
$ ./build/boehmprecise/clasp
Starting clasp-boehmprecise-2.5.0-115-g59d7eafc5 from base image
Resource file /home/sasank/.clasprc not found, skipping loading of it.
Top level in: #<PROCESS CORE::TOP-LEVEL @0x7f3f14dccf09 (Running)>.
COMMON-LISP-USER> (print "hello world")

"hello world" 
"hello world"
COMMON-LISP-USER>
```

## Simple FFI Example

CLASP is designed to interface with C++ and not just C. This is a big advantage because a lot of libraries can be taken advantage using this. They have [something called `clbind`](https://clasp-developers.github.io/clbind-doc.html) which is equivalent to LuaBind or pybind11. A deep dive into features of these is a whole another post. Let's set up build pipeline for interfacing using `clbind`.

Let's create a file in the `clasp` directory at the following location

```bash
mkdir -p extensions/my-demo/
touch extensions/my-demo/my-demo.cc
```

Put the following in `my-demo.cc`. I explain the code in comments.

```c++
// extensions/my-demo/my-demo.cc
#include <stdio.h>
// Required to have expose API available
#include <clasp/clasp.h>

// Function we are exposing
void my_func()
{
  printf("This is not the greatest function in the world. It's just a tribute!\n");
}

// This is a standard Common Lisp package
PACKAGE_USE("COMMON-LISP");
// Nickname of the package is MD
PACKAGE_NICKNAME("MD");
// MY-DEMO package corresponds to c++ name space md.
// MDPKg is used to refer to the package
NAMESPACE_PACKAGE_ASSOCIATION(md, MDPkg, "MY-DEMO");

// Our package will go into namespace to avoid conflicts
namespace md {
  // Preprocessor macro to identify the startup function
  CL_EXPOSE
  // This function name doesn't matter
  void my_demo_startup() {
    // executed on starting of clasp
    printf("Entered %s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ );
    // sc local variable is MDPkg defined earlier
    clbind::package_ sc(MDPkg);
    // Expose C++ my_func to lisp my-func
    sc.def("my-func", &my_func);
  }
}
```

Add another lisp file at `extensions/my-demo/cscript.lisp` so that builder identifies the source files.

```commonlisp
; extensions/my-demo/cscript.lisp
(k:sources :iclasp #~"my-demo.cc")
(k:systems :my-demo)
```

Finally we need to describe the package at `extensions/my-demo/my-demo.asd`

```commonlisp
; extensions/my-demo/cscript.lisp
(asdf:defsystem #:my-demo
  :description "My Demo"
  :version "0.0.1"
  :author "Sasank Chilamkurthy <sasankchilamkurthy@gmail.com>"
  :licence "LGPL-3.0"
  :depends-on ()
  :serial t)

```

Now we need to build this using `clasp`

```bash
./koga --extensions=my-demo
ninja -C build
```

You can verify if the extension works fine by running the following. You can see the print statement we have put in our startup function.

```
$ ./build/boehmprecise/clasp
Starting clasp-boehmprecise-2.5.0-115-g59d7eafc5 from extension image
Entered ../extensions/my-demo/my-demo.cc:17:my_demo_startup
Resource file /home/sasank/.clasprc not found, skipping loading of it.
Top level in: #<PROCESS CORE::TOP-LEVEL @0x7ff775b2af09 (Running)>.
COMMON-LISP-USER> (md:my-func)
This is not the greatest function in the world. It's just a tribute!
COMMON-LISP-USER>
```

We're building clasp extensions but these are not distributable as libraries. This is an issue because we want to be able to distribute our extension as libraries. But this might not be a real issue because very few people will be using clasp! It's as if we're built our version of lisp - which sounds cool but it really is not.

References:
1. [clasp wiki: building from source](https://github.com/clasp-developers/clasp/wiki/Building-and-Installing-from-Source)
2. [clbind manual](https://clasp-developers.github.io/clbind-doc.html)
3. [Clasp IRC channel](https://irclog.tymoon.eu/libera/%23clasp?from=1708435499)
4. [Github Clasp extension examples](https://github.com/clasp-developers/demo-clasp-cxx-interoperation)