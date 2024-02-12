---
layout: post
title: "Installing Drivers and Libraries for Intel Arc GPUs on Ubuntu"
author: Sasank Chilamkurthy
twitter_image: "https://chsasank.com/assets/images/scheme_compiler/ssa.png"
---

I've been working with Intel Arc GPUs for quite some time and want to document the driver and vendor library installation process in this tutorial. Unfortunately, installing these drivers neither is a single step nor are the packages small. My ultimate aim is to simplify these instructions considerably. Until then we have to make do with these steps.

## Distrobox

I prefer installing the drivers in a separate environment rather than directly on the system. That way I can roll back and not cause irreparable damage to my OS. This happened to me even when I installed well supported AMD drivers. For example, the system may boot to a black screen or display weird lines. I don't want to get into that situation and isolate the drivers and libraries to non-system environment.

Theoretically, [guix](https://guix.gnu.org/) is perfect for this use case. It allows me to do updates which can be rolled back, have [conda](https://conda.io/projects/conda/en/latest/user-guide/install/index.html) like environments and so on. However, guix mirrors are very slow to access from India -- so I can't really use it at the moment. In the future, I hope to maintain a mirror myself if guix fits my bill. It might also allow me to automate the entire process. Anyway, I have to do without it for now.

So, what's the alternative? I prefer docker based [distrobox](https://distrobox.it/). It basically uses containers to use different Linux distributions while automating mounting of home folders and other quality of life tricks. First, let's start by installing a container runtime. We can use docker or podman -- I recommend docker because most people have experience with it. I show my instructions to use convenience scripts but note that these can be dangerous.

Install docker:

```bash
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh
```

Add yourself to docker group so that you don't have to use sudo:

```bash
sudo groupadd docker
sudo usermod -aG docker $USER
newgrp docker
```

Next, install podman

```bash
curl -s https://raw.githubusercontent.com/89luca89/distrobox/main/install | sudo sh
```

Now let's create a ubuntu distro using a cheeky name.

```bash
distrobox create --name arc-reactor --image ubuntu:22.04
```

Let's now get into this new distro

```bash
distrobox enter arc-reactor
```

This might take a bit of time the first time because it'll install all the necessary packages. It will also ask you to set a password. You'll need this to use `sudo` inside the distrobox. Once all the setup is done, you should be seeing a prompt like the following:

```bash
(base) sasank@arc-reactor:~$ 
```

## GPU Drivers

Now we need to install GPU drivers for our Arc GPU. I am using Arc 370m GPU and we will follow the instructions from [here](https://dgpu-docs.intel.com/driver/client/overview.html#client-install-options). Let's start with ensuring our GPU is detected.

```bash
sudo apt update
sudo apt install pciutils
lspci -nn | grep -Ei 'VGA|DISPLAY'
```

Last command should output something like the following. Your machine might end up showing the device id other than `5693` based on [this table](https://dgpu-docs.intel.com/devices/hardware-table.html):

```
00:02.0 VGA compatible controller [0300]: Intel Corporation Device [8086:a7a0] (rev 04)
03:00.0 Display controller [0380]: Intel Corporation Device [8086:5693] (rev 05)
```

If successful, we will now add Intel's repositories to our distro:

```bash
wget -qO - https://repositories.intel.com/gpu/intel-graphics.key | \
  sudo gpg --dearmor --output /usr/share/keyrings/intel-graphics.gpg
echo "deb [arch=amd64,i386 signed-by=/usr/share/keyrings/intel-graphics.gpg] https://repositories.intel.com/gpu/ubuntu jammy client" | \
  sudo tee /etc/apt/sources.list.d/intel-gpu-jammy.list
sudo apt update
```

This will prompt for the password. Use the one you have set in the above step instead of the system password. Let us now install Intel runtimes:

```bash
sudo apt install -y \
  intel-opencl-icd intel-level-zero-gpu level-zero \
  intel-media-va-driver-non-free libmfx1 libmfxgen1 libvpl2 \
  libegl-mesa0 libegl1-mesa libegl1-mesa-dev libgbm1 libgl1-mesa-dev libgl1-mesa-dri \
  libglapi-mesa libgles2-mesa-dev libglx-mesa0 libigdgmm12 libxatracker2 mesa-va-drivers \
  mesa-vdpau-drivers mesa-vulkan-drivers va-driver-all vainfo hwinfo clinfo
```

Let's also install development packages:

```
sudo apt install -y \
  libigc-dev intel-igc-cm libigdfcl-dev libigfxcmrt-dev level-zero-dev
```

## Intel OneAPI

Now that we've installed the GPU drivers, let's proceed to install the vendor libraries. Intel's GPU library packages are labelled OneAPI. Key packages for my use are OneMKL and OneDNN for matrix operations and neural network operations, respectively. We need to add another set of Intel repos:

```bash
wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \ | gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null
echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list
sudo apt update
```

Then install the base toolkit which includes a compiler, matrix kernel library, deep neural networks library, and others.

```bash
sudo apt install intel-basekit
```

This will download about 2.5 GB of packages -- you might want to get a coffee or something. Next let's check if everything works fine by running a quick matrix multiplication benchmark:

```bash
sudo apt install git build-essential
git clone https://github.com/oneapi-src/oneAPI-samples.git
cd oneAPI-samples/Libraries/oneMKL/matrix_mul_mkl
source /opt/intel/oneapi/setvars.sh
make matrix_mul_mkl
```

Now let's do some benchmarks on single precision (fp32) matrix multiplication.

```
$ ./matrix_mul_mkl single 4096
oneMKL DPC++ GEMM benchmark
---------------------------
Device:                  Intel(R) Arc(TM) A370M Graphics
Core/EU count:           128
Maximum clock frequency: 2050 MHz

Benchmarking (4096 x 4096) x (4096 x 4096) matrix multiplication, single precision
 -> Initializing data...
 -> Warmup...
 -> Timing...

Average performance: 4.02554TF
```

On fp16 or half precision matrix multiplication:

```
$ ./matrix_mul_mkl half 4096
oneMKL DPC++ GEMM benchmark
---------------------------
Device:                  Intel(R) Arc(TM) A370M Graphics
Core/EU count:           128
Maximum clock frequency: 2050 MHz

Benchmarking (4096 x 4096) x (4096 x 4096) matrix multiplication, half precision
 -> Initializing data...
 -> Warmup...
 -> Timing...

Average performance: 10.5557TF
```

This should verify that we're indeed using a GPU because CPUs hardly reach 1 teraflops performance. That's all for this post folks. Hope this makes installing drivers for Intel GPUs easy. In a future post, I will benchmark [llama.cpp](https://github.com/ggerganov/llama.cpp) on Intel GPUs.