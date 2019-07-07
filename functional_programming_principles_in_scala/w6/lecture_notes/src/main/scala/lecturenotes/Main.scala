object w6 extends App {
  // 6.1 Other Collections
  val xs = Array(1,2,3,4)
  // println(xs map (x => x +1))

  val s = "Hello World"
  // println(s filter (c => c.isUpper))
  //
  def triangleNumbers(lowerBound: Int, upperBound: Int) = {
    (lowerBound to upperBound).flatMap(x => (x to upperBound).map(y => (x, y)))
  }

  // println(triangleNumbers(1, 10))

  println(s exists (c => c.isUpper))
  println(s forall (c => c.isUpper))
  val y = List(1,2,3).zip(s)
  println(y)
  println(y.unzip)
  println(s.flatMap (c => List('.', c)))
  println(xs.sum)
  println(xs.max)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
    (xs.zip(ys)).map(xy => xy._1 * xy._2).sum
  }

  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double = {
    (xs.zip(ys)).map{ case (x, y) => x * y }.sum
  }

  println(scalarProduct2(Vector(1.0,2.0),Vector(3.0,4.0)))

  def isPrime(n: Int): Boolean = (2 until n).forall(d => n % d != 0)

  println(isPrime(140))
}
