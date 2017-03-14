import timeit
import statistics

setup = """\
import numpy as np

a = np.random.randn(1000, 1000)
b = np.random.randn(1000, 1000)
aTa = np.matmul(np.transpose(a), a)
x = np.random.randn(1000000)
y = np.random.randn(1000000)

def sigmoid(z):
    return 1.0/(1.0+np.exp(-z))

def relu(x):
    return np.maximum(0, x)
"""

def bench(description, func):
    number = 100
    u = timeit.Timer(func, setup=setup)
    res = u.repeat(number=number, repeat=10)
    res = list(map(lambda x: x/float(number), res))
    mean = statistics.mean(res)
    median = statistics.median(res)
    stdev = statistics.stdev(res)
    print(description)
    print_seconds('\tmean', mean)
    print_seconds('\tmedian', median)
    print_seconds('\tstddev', stdev)

def print_seconds(desc, v):
    if v < 1:
        print_milliseconds(desc, 1000*v)
    else:
        print('%s: %.2f s' % (desc, v))

def print_milliseconds(desc, v):
    if v < 1:
        print_microseconds(desc, 1000*v)
    else:
        print('%s: %.2f ms' % (desc, v))

def print_microseconds(desc, v):
    if v < 1:
        print_nanoseconds(desc, 1000*v)
    else:
        print('%s: %.2f μs' % (desc, v))

def print_nanoseconds(desc, v):
    print('%s: %.2f ns' % (desc, v))


def main():
    bench('random matrix', 'np.random.randn(1000, 1000)')
    bench('inv', 'np.linalg.inv(a)')
    bench('Cholesky decomposition', 'np.linalg.cholesky(aTa)')
    bench('vector dot product', 'np.dot(x, y)')
    bench('matrix transpose', 'a.T')
    bench('matrix multiplication', 'np.dot(a, b)')
    bench('sigmoid', 'sigmoid(a)')
    bench('ReLU', 'relu(a)')

main()
