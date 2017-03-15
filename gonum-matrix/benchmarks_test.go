package benchmarks

import (
  "math/rand"
  "testing"
  "github.com/gonum/matrix/mat64"
  )

func BenchmarkRandMatrix(b *testing.B) {
  for i := 0; i < b.N; i++ {
    randMatrix(1000, 1000)
  }
}

func BenchmarkInverse(b *testing.B) {
  a := randMatrix(1000, 1000)
  b.ResetTimer()
  for i := 0; i < b.N; i++ {
    var ai mat64.Dense
    ai.Inverse(a)
  }
}

func BenchmarkCholesky(b *testing.B) {
  tmp := randMatrix(1000, 1000)
  // Construct a symmetric positive definite matrix.
  var a mat64.SymDense
  a.SymOuterK(1, tmp)

  b.ResetTimer()
  // Compute the cholesky factorization.
  for i := 0; i < b.N; i++ {
    var chol mat64.Cholesky
    chol.Factorize(&a)
  }
}

func BenchmarkDot(bench *testing.B) {
  x := randVector(1000000)
  y := randVector(1000000)
  bench.ResetTimer()
  for i := 0; i < bench.N; i++ {
    mat64.Dot(x, y)
  }
}

func BenchmarkTranspose(bench *testing.B) {
  a := randMatrix(1000, 1000)
  bench.ResetTimer()
  for i := 0; i < bench.N; i++ {
    a.T()
  }
}

func BenchmarkMul(bench *testing.B) {
  a := randMatrix(1000, 1000)
  b := randMatrix(1000, 1000)
  bench.ResetTimer()
  for i := 0; i < bench.N; i++ {
    var c mat64.Dense
    c.Mul(a, b)
  }
}

func BenchmarkSigmoid(bench *testing.B) {
  a := randMatrix(1000, 1000)
  bench.ResetTimer()
  for i := 0; i < bench.N; i++ {
    sigmoid(a)
  }
}

func BenchmarkRelu(bench *testing.B) {
  a := randMatrix(1000, 1000)
  bench.ResetTimer()
  for i := 0; i < bench.N; i++ {
    relu(a)
  }
}

func randMatrix(rows, cols int) *mat64.Dense {
  data := make([]float64, rows*cols)
  for i := range data {
    data[i] = rand.NormFloat64()
  }
  return mat64.NewDense(rows, cols, data)
}

func konstMatrix(rows, cols int, v float64) *mat64.Dense {
  data := make([]float64, rows*cols)
  for i := range data {
    data[i] = v
  }
  return mat64.NewDense(rows, cols, data)
}

func randVector(length int) *mat64.Vector {
  data := make([]float64, length)
  for i := range data {
    data[i] = rand.NormFloat64()
  }
  return mat64.NewVector(length, data)
}


func sigmoid(z *mat64.Dense) *mat64.Dense {
  rows, cols := z.Dims()
  one := konstMatrix(rows, cols, 1)
  var tmp mat64.Dense
  res := &tmp
  res.Scale(-1, z)
  res.Exp(res)
  res.Add(one, res)
  res.DivElem(one, res)
  return res
}

func relu(x *mat64.Dense) *mat64.Dense {
  reluElem := func(i, j int, v float64) float64 {
    if v < 0 {
      return 0
    } else {
      return v
    }
  }
  var tmp mat64.Dense
  res := &tmp
  res.Apply(reluElem, x)
  return res
}
