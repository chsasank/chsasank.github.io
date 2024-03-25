---
layout: post
title: "SYCL: A Portable Alternative to CUDA"
author: Sasank Chilamkurthy
twitter_image: 
---

As you probably know, many of the AI frameworks [depend on CUDA](https://chsasank.com/chicken-scheme-ffi-tutorial.html) to make AI work on GPUs. CUDA is a language extension to C that [only works](https://chsasank.com/nvidia-arm-aquisition-ai-explained.html) for Nvidia GPUs. People recognize this dependency and there is quite some work being done to create a portable alternative to CUDA. [ZLUDA](https://github.com/vosen/ZLUDA) recently made [news](https://www.techradar.com/pro/a-lone-developer-just-open-sourced-a-tool-that-could-bring-an-end-to-nvidias-ai-hegemony-amd-financed-it-for-months-but-abruptly-ended-its-support-nobody-knows-why) as a portable version of CUDA but it is not funded anymore. More reasonable and by now fairly well adopted alternative to CUDA comes from Khronos group called [SYCL](https://www.khronos.org/sycl/).

## SYCL

Khronos group is a non profit industry consortium that creates interoperability standards. Some of the major accomplishments include [OpenGL](https://en.wikipedia.org/wiki/OpenGL) and [Vulkan](https://en.wikipedia.org/wiki/Vulkan) for gaming/graphics. Every major game engine implements one of these two these days. OpenCL is an attempt from them to create a similar standard for computation. Its history is as old as CUDA itself. Unfortunately, it was not able to be force for the computing industry like OpenGL/Vulkan has been for gaming. This is mainly because OpenCL is very low level and many useful abstractions are missing.

[SYCL](https://en.wikipedia.org/wiki/SYCL) is a sort of successor to OpenCL with useful abstractions and is designed to be implemented for all GPUs. It is a reference standard instead of implementation. In other words, its an API specification. Unlike CUDA, it is not an extension to C or C++. Instead, it is a pure C++ embedded DSL that works based on templates and headers. It has amazing abstractions including unified shared memory, command queues, lambdas as kernels and lot more. A great book to learn more about SYCL is [available for free](https://link.springer.com/book/10.1007/978-1-4842-5574-2).

<figure>
<label for="mn-fig-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-1" class="margin-toggle">
<span class="marginnote">SYCL and some implementations</span>
<img src="https://www.khronos.org/assets/uploads/apis/2020-05-sycl-landing-page-02a_2.jpg" alt="SYCL and some of its implementations">
</figure>

Since SYCL is an API spec/standard, there are multiple projects implementing SYCL. Some notable ones include [DPC++](https://github.com/intel/llvm), [AdaptiveCPP](https://github.com/AdaptiveCpp/AdaptiveCpp) and [ComputeCPP](https://developer.codeplay.com/products/computecpp/ce/home/). DPC++ is Intel's SYCL implementation that works for most of their devices along with Nvidia/AMD cards. I have earlier [used it](https://chsasank.com/portblas-portable-blas-across-gpus.html) and benchmarked BLAS across different GPUs. It actually works and performs quite well! This proved to me that a portable implementation of CUDA is indeed possible. While well documented, I did not like the behemoth of the codebase too much -- after all it's a fork of LLVM monorepo.

## AdaptiveCPP

Enter AdaptiveCPP! Formerly it was called hipSYCL because it started as an implementation targeting AMD GPUs in addition to Nvidia GPUs. However, it is not actually a product of AMD and is actually a project by [Aksel Alpay](https://emcl.iwr.uni-heidelberg.de/people/alpay-aksel) from [University of Heidelberg](https://en.wikipedia.org/wiki/Heidelberg_University). The fact that it's a project by single person meant the architecture had to be kick ass and codebase sane. This is in contrast to Intel's implementation where they can afford to throw people at the problem. AdaptiveCPP caught my eye when the [latest release](https://github.com/AdaptiveCpp/AdaptiveCpp/releases/tag/v24.02.0) started to outperform Intel's DPC++.

I was really sold on AdaptiveCPP (`acpp` henceforth) when I [read](https://dl.acm.org/doi/abs/10.1145/3585341.3585351) that it can now do single source, single pass compilation! Importance of this is hard to understate if you understand how other SYCL compilers work. While SYCL standard requires you to have single source to be shared for host and device, compilers can implement multiple passes on the same source. For example, DPC++ compiles same SYCL file twice: once for the CPU and once for the GPU. DPC++ later links the host binary with device binary and creates a fat binary.

AdaptiveCPP abstracted out this requirement and created two stages of compilation -- one for ahead of time (AOT) and another for run time (RT). AOT stage parses the SYCL source file in one single go and creates an intermediate representation to be passed to RT stage. This representation is independent of devices and is translated to actual device code during runtime at RT stage. This design makes the compilations significantly faster but it is also aesthetic pleasing. In other words, `acpp` can do just-in-time or JIT compilation. See the below figure for comparison between `acpp` (figure 1, SSCP) and DPC++ (figure 2, SMCP):

<figure>
<label for="mn-fig-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-1" class="margin-toggle">
<span class="marginnote">Comparison between single source single compiler pass and single source multiple compiler pass. [Source](https://dl.acm.org/doi/abs/10.1145/3585341.3585351)</span>
<img src="/assets/images/random/acpp-sscp-ssmp.png" alt="Comparison between single source single compiler pass and single source multiple compiler pass">
</figure>

### Installing AdaptiveCPP

As usual, let's distrobox to create a new environment and install acpp and its dependencies inside. We'll try to keep the dependencies minimal to understand what's really required.

```bash
distrobox create --name acpp-reactor --image ubuntu:22.04
distrobox enter acpp-reactor
```

Let's start with installing LLVM. We'll follow instructions from [here](https://github.com/AdaptiveCpp/AdaptiveCpp/blob/develop/doc/install-llvm.md).

```bash
sudo apt update && sudo apt install -y lsb-release wget software-properties-common gnupg
wget https://apt.llvm.org/llvm.sh #Convenience script that sets up the repositories
chmod +x llvm.sh
sudo ./llvm.sh 16 #Set up repositories for clang 16
sudo apt update && sudo apt install -y libclang-16-dev clang-tools-16 libomp-16-dev llvm-16-dev lld-16
```

Next let's install other dependencies of `acpp`:

```bash
sudo apt install -y python3 cmake libboost-all-dev git build-essential
```

It's time to install drivers for our GPU. I use Intel Arc 770 for this build. For other devices, you can follow instructions from my [earlier post](https://chsasank.com/portblas-portable-blas-across-gpus.html):

```bash
# Drivers for Intel GPUs
wget -qO - https://repositories.intel.com/gpu/intel-graphics.key | \
  sudo gpg --dearmor --output /usr/share/keyrings/intel-graphics.gpg
echo "deb [arch=amd64,i386 signed-by=/usr/share/keyrings/intel-graphics.gpg] https://repositories.intel.com/gpu/ubuntu jammy client" | \
  sudo tee /etc/apt/sources.list.d/intel-gpu-jammy.list
sudo apt update
sudo apt install -y \
  intel-opencl-icd intel-level-zero-gpu level-zero \
  intel-media-va-driver-non-free libmfx1 libmfxgen1 libvpl2 \
  libegl-mesa0 libegl1-mesa libegl1-mesa-dev libgbm1 libgl1-mesa-dev libgl1-mesa-dri \
  libglapi-mesa libgles2-mesa-dev libglx-mesa0 libigdgmm12 libxatracker2 mesa-va-drivers \
  mesa-vdpau-drivers mesa-vulkan-drivers va-driver-all vainfo hwinfo clinfo xpu-smi
sudo apt install -y \
  libigc-dev intel-igc-cm libigdfcl-dev libigfxcmrt-dev level-zero-dev
```

Now clone `acpp` and build it:

```bash
git clone https://github.com/AdaptiveCpp/AdaptiveCpp
cd AdaptiveCpp
mkdir build && cd build
cmake ..
make -j
sudo make install
```

Now check the available devices using 

```bash
$ acpp-info -l
=================Backend information===================
Loaded backend 0: OpenMP
  Found device: hipSYCL OpenMP host device
Loaded backend 1: Level Zero
  Found device: Intel(R) Arc(TM) A770 Graphics
```
