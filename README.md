# Linear Algebra Performance Benchmarks

## List of benchmarks

1. Create a random 1000x1000 matrix.

2. Invert a 1000x1000 random matrix.

3. Compute the Cholesky decomposition of a random 1000x1000 matrix.

4. Vector (1 000 000) dot product.

5. Matrix (1000x1000) transpose.

6. Matrix (1000x1000 <> 1000x1000) multiply.

## Results

| Benchmark | hmatrix | numpy |
|-----------------|----------------|-----------------|
| 1. Random matrix | **16.10 ms**  | 39.24 ms |
| 2. Invert matrix | 84.85 ms  | 84.74 ms |
| 3. Cholesky decomposition | 28.74 ms | 29.40 ms |
| 4. Vector dot product | 1.965 ms | **0.80 ms** |
| 5. Matrix transpose | **41.80 ns** | 472.0 ns |
| 6. Matrix multiply | **32.78 ms** | 37.01 ms |
