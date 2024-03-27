---
layout: post
title: "Compiling the Compiler for Intel GPUs"
author: Sasank Chilamkurthy
twitter_image: 
---

Building anything is one of the best ways to learn about something. I am looking to understand the internals of DPC++ compiler: so let's build it. Idea is to identify the exact dependencies of this system. As usual, we will use distrobox to create our environment.


```
distrobox create --name sycl-reactor --image ubuntu:22.04
distrobox enter sycl-reactor
```

Let's install the dependencies:

```
sudo apt update
sudo apt install build-essential git cmake ninja-build python3 pkg-config
```

Let's create a folder for all our work and clone sources of DPC++ compiler

```
mkdir sycl_workspace
cd sycl_workspace
export DPCPP_HOME=`pwd`
git clone https://github.com/intel/llvm -b sycl
```

Let's compile using simple python commands

```
python $DPCPP_HOME/llvm/buildbot/configure.py
python $DPCPP_HOME/llvm/buildbot/compile.py
```

Wouldn't it be so nice if I could use guix for this? Well let's do that after I figured out basic installation.

## Compute Runtime

Now that we have compiled our compiler, we need to get it a runtime. We will use Level Zero runtime: https://github.com/intel/compute-runtime.git. Again let's build this from source.
Level zero is a spec - basically headers. Intel implemented a version for their GPU but codeplay has plugins for Nvidia and AMD GPUs as well.

First verify that no SYCL supported devices are available right now:

```bash
$ cd $DPCPP_HOME
$ ./llvm/build/bin/sycl-ls --verbose

Platforms: 0
default_selector()      : No device of requested type available. -1 (PI_ERRO...
accelerator_selector()  : No device of requested type available. -1 (PI_ERRO...
cpu_selector()          : No device of requested type available. -1 (PI_ERRO...
gpu_selector()          : No device of requested type available. -1 (PI_ERRO...
custom_selector(gpu)    : No device of requested type available. -1 (PI_ERRO...
custom_selector(cpu)    : No device of requested type available. -1 (PI_ERRO...
custom_selector(acc)    : No device of requested type available. -1 (PI_ERRO...
```

Before we compile Intel's level zero runtime, we need to build its dependencies: Intel graphics compiler and graphics memory management

### Intel Graphics Compiler

First dependencies:

```
sudo apt-get install flex bison libz-dev cmake libc6 libstdc++6 python3-pip
sudo python3 -m pip install mako
```

Of course we will build the dependencies of this as well. Let's make a home dir for all this

```bash
mkdir ~/igc_workspace
cd ~/igc_workspace
git clone https://github.com/intel/vc-intrinsics vc-intrinsics
git clone -b llvmorg-14.0.5 https://github.com/llvm/llvm-project llvm-project
git clone -b ocl-open-140 https://github.com/intel/opencl-clang llvm-project/llvm/projects/opencl-clang
git clone -b llvm_release_140 https://github.com/KhronosGroup/SPIRV-LLVM-Translator llvm-project/llvm/projects/llvm-spirv
git clone https://github.com/KhronosGroup/SPIRV-Tools.git SPIRV-Tools
git clone https://github.com/KhronosGroup/SPIRV-Headers.git SPIRV-Headers
```

Now clone the desired version of Intel Graphics Compiler. I have picked this version from the [releases doc](https://github.com/intel/compute-runtime/releases) of Intel's compute runtime.

```
IGC_TAG=igc-1.0.15985.7
git clone https://github.com/intel/intel-graphics-compiler --branch $IGC_TAG igc
```

Write notes on what each of these do.

Now time to build

```
mkdir build && cd build
cmake ../igc
make -j`nproc`
```

and install it using

```
sudo make install
```

This should intel graphics assembler into `/usr/local/bin`

```bash
$ iga64 
Intel Graphics Assembler 1.1.0-6cc111d26
usage: iga64 OPTIONS ARGS
where OPTIONS:
  -h     --help                       shows help on an option
  -d     --disassemble                disassembles the input file
  -a     --assemble                   assembles the input file
         --color           COLORING   colors assembly output ('always', 'never', and 'auto')
...
```

### Graphics Memory Management

Now time to build memory management library. Again, picked the version number from Intel's version docs

```bash
GMM_TAG=intel-gmmlib-22.3.11
git clone https://github.com/intel/gmmlib.git -b $GMM_TAG
```

Let's build

```bash
cd gmmlib
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j"$(nproc)"
```

Install

```
sudo make install
```

### Compute Runtime

All dependencies are finally built. Now time to build runtime.

```bash
mkdir compute_workspace && cd compute_workspace
CRT_TAG=24.05.28454.6
git clone https://github.com/intel/compute-runtime -b $CRT_TAG neo
```

Now build:

```bash
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release -DNEO_SKIP_UNIT_TESTS=1 ../neo
make -j`nproc`
```

Ah man, the last one failed :(


References:
1. https://github.com/intel/llvm/blob/sycl/sycl/doc/GetStartedGuide.md
2. https://github.com/intel/compute-runtime/blob/master/BUILD.md
3. https://github.com/intel/intel-graphics-compiler/blob/master/documentation/build_ubuntu.md
4. https://github.com/intel/gmmlib