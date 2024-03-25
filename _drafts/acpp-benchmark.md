
## Benchmarking `acpp`

To benchmark how well this compiles, let's build [portBLAS](https://chsasank.com/portblas-portable-blas-across-gpus.html) library with `acpp`. Let's clone the portBLAS library and make a build directory.

```bash
git clone --recursive https://github.com/codeplaysoftware/portBLAS.git
cd portBLAS
mkdir build && cd build
```

Install dependencies for PortBLAS:

```bash
sudo apt install -y ninja-build libopenblas-dev
```

Manually remove the following lines which disable benchmarking for `acpp` in `cmake/Modules/SYCL/cmake`:

```cmake
  if((${BLAS_ENABLE_BENCHMARK}) AND (${TUNING_TARGET} IN_LIST HIP_BENCH_UNSUPPORTED_TARGETS))
    message(STATUS "Benchmarks are not supported when targetting OpenCL/LevelZero backend 
            devices. portBLAS Benchmarks are disabled.")
    set(BLAS_ENABLE_BENCHMARK OFF)
  endif()
```

Let's build it:

```bash
export CC=clang-16
export CXX=acpp
export ACPP_TARGETS=generic
# select the right target
cmake -GNinja -DTUNING_TARGET=INTEL_GPU -DCMAKE_BUILD_TYPE=Release \
      -DSYCL_COMPILER=adaptivecpp -DACPP_TARGETS=$ACPP_TARGETS \
      -DBLAS_ENABLE_BENCHMARK=ON ..
ninja
```