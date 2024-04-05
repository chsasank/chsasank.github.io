#include <sycl/sycl.hpp>

using namespace sycl;

void vector_add(float* A, float *B, float *C, int n){
    // Create buffers for A, B, and C
    buffer<float> bufA(A, n);
    buffer<float> bufB(B, n);
    buffer<float> bufC(C, n);

    // Create a SYCL queue to submit work to
    queue q;
    // Submit a command group to the queue
    q.submit([&](handler& h) {
        // Create accessors for buffers
        auto accA = bufA.get_access<access::mode::read>(h);
        auto accB = bufB.get_access<access::mode::read>(h);
        auto accC = bufC.get_access<access::mode::write>(h);

        // Define the kernel
        h.parallel_for(range<1>(n), [=](id<1> i) {
            accC[i] = accA[i] + accB[i];
        });
    });

    // Ensure all work is completed
    q.wait();
}