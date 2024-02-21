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
