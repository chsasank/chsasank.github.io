## BLIS

BLIS is a BLAS-like library but somewhat cooler. Let's have our scheme do FFI with it! Let's start by compiling it

```
git clone https://github.com/flame/blis.git
cd blas
./configure auto
make -j
make check -j
sudo make install
```

But it is not great for me because 

1. No GPU support
2. No quantization support


# PortBLAS

## On Nvidia GPUs:

git clone --recursive https://github.com/codeplaysoftware/portBLAS.git
cd portBLAS
mkdir build
cd build
export CC=/opt/intel/oneapi/2024.0/bin/compiler/clang
export CXX=/opt/intel/oneapi/2024.0/bin/compiler/clang++
cmake -GNinja ../ -DSYCL_COMPILER=dpcpp -DDPCPP_SYCL_ARCH=sm_75 -DDPCPP_SYCL_TARGET=nvptx64-nvidia-cuda -DTUNING_TARGET=NVIDIA_GPU -DCMAKE_BUILD_TYPE=Release -DBUILD_CUBLAS_BENCHMARKS=ON
ninja


n,n,2048,2048,2048,1,0
n,t,2048,2048,2048,1,0
t,n,2048,2048,2048,1,0
t,t,2048,2048,2048,1,0

./benchmark/portblas/bench_gemm --csv-param params.csv --benchmark_out=../results.json --benchmark_out_format=json  --benchmark_format=console


## On Intel GPUs

git clone --recursive https://github.com/codeplaysoftware/portBLAS.git
cd portBLAS
mkdir build
cd build
export CC=/opt/intel/oneapi/2024.0/bin/compiler/clang
export CXX=/opt/intel/oneapi/2024.0/bin/compiler/clang++
cmake -GNinja ../ -DSYCL_COMPILER=dpcpp -DTUNING_TARGET=INTEL_GPU -DCMAKE_BUILD_TYPE=Release
ninja

$ cat params.csv
n,n,2048,2048,2048,1,0
n,t,2048,2048,2048,1,0
t,n,2048,2048,2048,1,0
t,t,2048,2048,2048,1,0
n,n,4096,4096,4096,1,0
n,n,5792,5792,5792,1,0




./benchmark/portblas/bench_gemm --csv-param params.csv --benchmark_out=../results.json --benchmark_out_format=json --benchmark_format=console

kernel, based on real_time, bench
BM_Gemm<float>/n/n/4096/4096/4096/buffer/real_time 8383 8384
BM_Gemm<float>/n/t/4096/4096/4096/buffer/real_time 5640 5641
BM_Gemm<float>/t/n/4096/4096/4096/buffer/real_time 4076 4076
BM_Gemm<float>/t/t/4096/4096/4096/buffer/real_time 4229 4230
BM_Gemm<float>/n/n/4096/4096/4096/usm/real_time 8217 8218
BM_Gemm<float>/n/t/4096/4096/4096/usm/real_time 4303 4303
BM_Gemm<float>/t/n/4096/4096/4096/usm/real_time 3405 3405
BM_Gemm<float>/t/t/4096/4096/4096/usm/real_time 3525 3525
BM_Gemm<complex<float>>/n/n/4096/4096/4096/buffer/real_time 628 2513
BM_Gemm<complex<float>>/n/t/4096/4096/4096/buffer/real_time 982 3929
BM_Gemm<complex<float>>/t/n/4096/4096/4096/buffer/real_time 607 2428
BM_Gemm<complex<float>>/t/t/4096/4096/4096/buffer/real_time 613 2454
BM_Gemm<complex<float>>/n/n/4096/4096/4096/usm/real_time 583 2332
BM_Gemm<complex<float>>/n/t/4096/4096/4096/usm/real_time 898 3593
BM_Gemm<complex<float>>/t/n/4096/4096/4096/usm/real_time 566 2266
BM_Gemm<complex<float>>/t/t/4096/4096/4096/usm/real_time 549 2197