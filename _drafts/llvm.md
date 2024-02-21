```
sudo apt install -y build-essential git cmake ninja-build ccache python3-pybind11
git clone https://github.com/intel/llvm.git
mkdir llvm/build
cd llvm/build
cmake -G Ninja ../llvm \
    -DLLVM_ENABLE_PROJECTS=mlir\;clang\;lldb \
    -DLLVM_ENABLE_RUNTIMES=libcxxabi\;libcxx \
    -DLLVM_BUILD_EXAMPLES=ON \
    -DLLVM_TARGETS_TO_BUILD="Native;NVPTX;AMDGPU" \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_CCACHE_BUILD=ON \
    -DLLVM_ENABLE_FFI=ON -DLLVM_ENABLE_LIBCXX=ON -DLLVM_ENABLE_RTTI=ON \
    -DLLVM_ENABLE_EH=ON -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_ENABLE_ASSERTIONS=ON
cmake --build . --target check-mlir
sudo cmake --build . --target install
```

https://mlir.llvm.org/getting_started/

https://github.com/clasp-developers/clasp/wiki/Building-LLVM-from-Source



Now let's get into binding our fibonacci number code

```
/* fib.c */
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



