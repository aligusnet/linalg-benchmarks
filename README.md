# Linear Algebra Performance Benchmarks

## List of benchmarks

1. Create a random 1000x1000 matrix.

2. Invert a 1000x1000 random matrix.

3. Compute the Cholesky decomposition of a random 1000x1000 matrix.

4. Vector (1 000 000) dot product.

5. Matrix (1000x1000) transpose.

6. Matrix (1000x1000 <> 1000x1000) multiply.

7. Sigmoid

8. ReLU

9. Sum by rows

10. sum by columns

11. indices of max elements in rows

12. indices of max elements in columns

## Results

* hmatrix: 0.18.0.0 (ghc: 8.0.2) & hmatrix-morpheus: 0.1.0.0
* numpy: 1.11.1 (python: 3.5.2)
* octave: 4.0.3
* gonum matrix: master (go: 1.8)
* LASwift: 0.1.2 (Apple Swift 3.0.2)

| Benchmark | hmatrix (hmatrix-morpheus) | numpy | octave | gonum matrix | LASwift |
|-----------------|----------------|-----------------|-------|-------|-------|
| 1. Random matrix | 16.10 ms  | 39.24 ms | 18.48 ms | 41.98 ms | 15 ms |
| 2. Invert matrix | 84.85 ms  | 84.74 ms | 57.00 ms | 364.49 ms | 39 ms |
| 3. Cholesky decomposition | 28.74 ms | 29.40 ms | 9.11 ms | 116.60 ms | 33 ms |
| 4. Vector dot product | 1.965 ms | 0.80 ms | 1.07 ms | 0.990 ms | 1 ms |
| 5. Matrix transpose | 41.80 ns | 262.0 ns | 22,860 ns | 0.65 ns | 11e9 ns |
| 6. Matrix multiply | 32.78 ms | 37.01 ms | 36.38 ms | 185.65 ms | 30 ms |
| 7. Sigmoid | 14.40 ms | 19.76 ms | 18.14 ms | 2 333 ms | 20 ms |
| 8. ReLU | 3.575 ms | 7.39 ms | 3.11 ms | 10.5 ms | 4 ms |
| 9. sum by rows | 3.276 ms (1.069 ms) | 0.546 ms | 0.830 ms | | 2 ms |
| 10. sum by columns | 14.690 ms (0.539 ms) | 0.692 ms | 1.049 ms | | 13 ms |
| 11. max index by rows | 1.540 ms (0.830 ms) |  0.924 ms | 1.131 ms | | 2 ms |
| 12. max index by columns | 12.980 ms (0.902 ms) |  9.940 ms | 1.460 ms | | 13 ms |

## TODO:

* dlib (http://dlib.net)
* eigen (http://eigen.tuxfamily.org/)
* MKL backend
