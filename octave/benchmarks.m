repeats = 10;
iters = 10;

A = randn(1000, 1000);
B = randn(1000, 1000);
AtA = A'*A;
x = randn(1000, 1000)(:);
y = randn(1000, 1000)(:);

bench(repeats, iters, 'rand matrix', @(t)(randn(t, 1000)), 1000)
bench(repeats, iters, 'inv', @inv, A);
bench(repeats, iters, 'Cholesky decomposition', @chol, AtA);
bench(repeats, iters, 'vector dot product', @(t)(dot(t, y)), x);
bench(repeats, iters, 'matrix transpose', @transpose, x);
bench(repeats, iters, 'matrix multiply', @(t)(mult(t, A)), B);
bench(repeats, iters, 'sigmoid', @sigmoid, B);
bench(repeats, iters, 'relu', @relu, B);
