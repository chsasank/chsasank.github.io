---
layout: post
title: "Intermediate Representations for GPUs"
author: Sasank Chilamkurthy
twitter_image: 
---

It's been a month since I last wrote a blog post. I usually write when I feel I made a good progress but last month has been like hitting a wall. Compilers are like dragons and it's been hard to get my head around the complexity. To make things worse, I have picked a relatively hard topic even amidst this complexity: AI compilers. What separates AI from others are GPUs and matrix multiplication kernels. In this post, I will talk about compilers for GPUs and will leave matrix multiplication kernels to another post. This post will be organized as followings:

1. State of LLVM for GPUs
2. Characteristics of an ideal intermediate representation (IR)
3. Implementation options for a GPU IR

## LLVM doesn't cut it for GPUs

The title is probably going to be a controversial statement but I will explain why. A good review of architecture of LLVM can be found in the book [The Architecture of Open Source Applications](https://aosabook.org). I reproduce a key diagram from the [LLVM chapter](https://aosabook.org/en/v1/llvm.html) below for reference:

<figure>
<label for="mn-fig-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-1" class="margin-toggle">
<span class="marginnote">LLVM Architecture. [Source](https://aosabook.org/en/v1/llvm.html)</span>
<img src="https://aosabook.org/static/llvm/LLVMCompiler1.png
" alt="LLVM Architecture">
</figure>

Original design for LLVM was that any programming language implementation can translate code into LLVM intermediate representation (IR) and the library will take care of optimizations and generating the assembly code for the target architecture. In other words, backend is automated by the LLVM library assuming you follow the IR contract. This design does work for CPUs -- x86, ARM and PowerPC and what not. Programming language implementors need not worry about the details of device architecture. They can restrict their attention to what is called as frontend.

To examine this, let's take the following vector add program:

```c
// vector-add.c
void vector_add(float *a, float *b, float *c, int n){
    int i;
    for (i = 0; i < n; i++) {
        c[i] = a[i] + b[i];
    }
}
```

I wrote a small driver that initializes `a[i] = i`, `b[i] = 2 * i`, `n = 10` and computes sum of elements of `c`. Let's first compile `vector-add.c` to LLVM IR and then link it the driver.

```bash
# on my x86 machine
# frontend compiles to llvm
$ clang -S -emit-llvm vector-add.c
# remove any metadata about target
$ sed  -i '/target/d'  vector-add.ll 
# backend
$ clang driver.c vector-add.ll
warning: overriding the module target triple with x86_64-pc-linux-gnu [-Woverride-module]
1 warning generated.
# final binary
$ ./a.out
sum of element of C is: 135.000000
```

To verify the frontend's independence from backend, I remove any metadata about the target device in `vector-add.ll` and then transfer it to my mac. My mac uses M2 chip which is an Arm based architecture. On my mac:

```bash
# On my mac (i.e arm machine)
$ clang driver.c vector-add.ll
warning: overriding the module target triple with arm64-apple-macosx13.0.0 [-Woverride-module]
1 warning generated.
$ ./a.out 
sum of element of C is: 135.000000
```

This design, unfortunately, doesn't work for GPUs. I don't mean to say that such a model can not be implemented for GPUs. Instead, I mean to say that LLVM doesn't actually implement it for GPUs. This is because LLVM depends on builtins that are specific to each architecture. It has become the job of a frontend to use the right builtins and conventions for your target GPU. Therefore, generating LLVM IR doesn't get you backend for all GPUs for free like it works for CPUs.

To examine this, we'll write a simple vector-add program in SYCL and compile it with DPC++, which is Intel's fork of LLVM that works for all GPUs. I have shown how to set it up for different architectures in a [previous post](https://chsasank.com/portblas-portable-blas-across-gpus.html). To recap, a single source code written in SYCL can compile to all GPUs if we use DPC++.

