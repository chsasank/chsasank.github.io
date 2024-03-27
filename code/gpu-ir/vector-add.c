void vector_add(float *a, float *b, float *c, int n){
    int i;
    for (i = 0; i < n; i++) {
        c[i] = a[i] + b[i];
    }
}