import timeit

setup = """\
import numpy as np

a = np.random.randn(1000, 1000)
b = np.random.randn(1000, 1000)
aTa = np.matmul(np.transpose(a), a)
x = np.random.randn(1000000)
y = np.random.randn(1000000)
"""

def bench(description, func):
    number = 100
    u = timeit.Timer(func, setup=setup)
    print(description, 1000*u.timeit(number=number)/number, 'ms')


bench('random matrix', 'np.random.randn(1000, 1000)')
bench('inv', 'np.linalg.inv(a)')
bench('Cholesky decomposition', 'np.linalg.cholesky(aTa)')
bench('vector dot product', 'np.dot(x, y)')
bench('matrix transpose', 'a.T')
bench('matrix multiplication', 'np.dot(a, b)')
