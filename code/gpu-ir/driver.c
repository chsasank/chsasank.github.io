#include <stdio.h>

extern void vector_add(float *a, float *b, float *c, int n);

int main(){
    int n = 10;
    float a[n], b[n], c[n];
    int i;
    for (i = 0; i < n; i++){
        a[i] = i;
        b[i] = 2 * i;
    }
    vector_add(a, b, c, n);
    float sum = 0;
    for (i = 0; i < n; i++){
        sum += c[i];
    }
    printf("sum of element of C is: %f\n", sum);
}
