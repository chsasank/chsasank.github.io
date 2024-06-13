---
layout: post
title: "MLIR Generation"
author: Sasank Chilamkurthy
twitter_image: 
---

The reason I am working on FFI is because I want to use MLIR C++ library. I have C bindings which are used to create python bindings as well. But I will not be able to create new dialects or transformations through this API. But MLIR is a complicated library which is highly verbose. If you go through the basic 'Toy' language tutorial, you will understand what I mean.

MLIR also supports a textual format which is semantically identical to in-memory representation. Why should I not directly model this textual format and actually generate working MLIR code? This way I can remain a light weight compiler and still take the advantage of all the ecosystem built around MLIR. I didn't believe that this approach might work but then I read about [xDSL paper](https://arxiv.org/abs/2311.07422) about what they call side kick compiler. The authors have been to able to create python-native compiler toolkit while taking advantage of MLIR ecosystem.

This approach is not unlike using MLIR as sort of an assembly language. My earlier [blog series](https://chsasank.com/scheme-compiler-4-binary-primitives.html) literally generated text x86 assembly code. A [course](https://www.cs.cornell.edu/courses/cs6120/2020fa/) I used to learn about compiler optimizations used a json based [representation](https://github.com/sampsyo/bril) called [bril](https://capra.cs.cornell.edu/bril/). I have actually written a [few passes](https://github.com/chsasank/compiler-optimization-examples) manipulating bril. It was refreshingly easy: I could use python and write nice little optimization passes.

At this point of time, the ability to rapidly prototype my compiler is more important than performance. Besides the performance I care about right now is the performance of the created binary. Generating MLIR using a high level language like python or scheme might make my compiler slow. That is, generating binary might be slow. However, this is a non-concern to me as long as the compile time is reasonable.

Another advantage of this system is that I will be able to separate compiler from runtime. This will make my system modular and allow me to add different runtimes easily. I should be able to use all the optimizations in mlir at compile time through simple subprocess. I have already got such a scaffolding running [earlier](https://github.com/chsasank/scheme-incremental-compiler/tree/main).

## Intel's MLIR Extensions

Ok enough talk! Let's do. My aim is to generate matmul kernels that run as fast as hand-optimized kernels on Intel GPUs. Intel recently released their MLIR extensions and XeGPU dialect which closely mirrors the instruction set of the GPU. In that they have some examples for [benchmark of transpose](https://github.com/intel/mlir-extensions/tree/main/benchmarks/transpose) written in MLIR code. So if we are able to build everything and make that work, I'd consider that a win!

Let's start with creating a new distrobox for experimenting with all our stuff. Follow instructions from my [earlier post](https://chsasank.com/intel-arc-gpu-driver-oneapi-installation.html) to create the `arc-reactor` distrobox.

```
distrobox enter arc-reactor
```

<!-- TODO: install the bare minimum stuff possible -->

First clone the repositories we need:

```bash
git clone https://github.com/intel/mlir-extensions.git
git clone https://github.com/llvm/llvm-project.git
```

We need to checkout the exact LLVM version required by Intel's mlir extensions and apply a few custom LLVM patches

```bash
cd llvm-project
git checkout `cat ../mlir-extensions/build_tools/llvm_version.txt`
git apply ../mlir-extensions/build_tools/patches/*
```

Next let's build using [LLVM option](https://llvm.org/docs/CMake.html) `LLVM_EXTERNAL_PROJECTS`. We will enable GPU support using options `IMEX_ENABLE_L0_RUNTIME` and `IMEX_ENABLE_SYCL_RUNTIME`. We will also build benchmarks using the option `IMEX_ENABLE_BENCHMARK`.

```bash
cmake -G Ninja -B build -S llvm \
   -DLLVM_ENABLE_PROJECTS=mlir \
   -DLLVM_BUILD_EXAMPLES=ON \
   -DLLVM_TARGETS_TO_BUILD="X86" \
   -DCMAKE_BUILD_TYPE=Release \
   -DLLVM_ENABLE_ASSERTIONS=ON \
   -DLLVM_EXTERNAL_PROJECTS="Imex" \
   -DIMEX_ENABLE_L0_RUNTIME=ON \
   -DIMEX_ENABLE_SYCL_RUNTIME=ON \
   -DIMEX_ENABLE_BENCHMARK=ON \
   -DLLVM_EXTERNAL_IMEX_SOURCE_DIR=../mlir-extensions
cmake --build build --target check-imex
```

Get a coffee while LLVM/MLIR is being built. You might see a few tests fail - ignore them for now.

References:
1. https://github.com/intel/mlir-extensions/tree/main?tab=readme-ov-file#building-imex