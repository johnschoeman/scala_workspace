package streamnotes

class StreamNotes {
  def streamRange(lo: Int, hi: Int): Stream[Int] = {
    print(lo+" ")
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))
  }

  def listRange(lo: Int, hi: Int): List[Int] = {
    if (lo >= hi) Nil
    else lo :: listRange(lo + 1, hi)
  }

  // println(streamRange(1, 10).take(8).toList)

  def from(n: Int): Stream[Int] = n #:: from(n + 1)

  // println(from(10).take(3).toList)

  val nats = from(0)
  val m4s = nats map (_ * 4)
  // println(m4s take 10 toList)

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0))

  val primes = sieve(from(2))
  // println(primes.take(100).toList)

  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }

  def isGoodEnough(guess: Double, x: Double) =
    math.abs((guess * guess - x) / x) < 0.0001

  // println(sqrtStream(4).take(10).toList)
  // println(sqrtStream(4).filter(isGoodEnough(_, 4)).take(10).toList)

  def fib(a: Int, b: Int): Stream[Int] = {
    print(".")
    a #:: fib(b, a + b)
  }

  def gRat(i: Int): Stream[Double] = {
    val fibMem = fib(1, 1).take(i + 1).toList
    (fibMem(i).toDouble / fibMem(i - 1)) #:: gRat(i + 1)
  }

 val fibMem = fib(1, 1)
  def gRat2(i: Int, fibMem: => Stream[Int]): Stream[Double] = {
    (fibMem(i).toDouble / fibMem(i - 1)) #:: gRat2(i + 1, fibMem)
  }

  // println(fib(-1,1).take(19).toList)
  // println(gRat(2).take(10).toList)
  // println(gRat2(2, fibMem).take(10).toList)
  // println(fibMem(10))

  // def nestedStream(n: Int): Stream[Stream[Int]] = {
  // }

  def flattenStream[T](xs: Stream[Stream[T]]): Stream[T] = {
    val initialAcc: Stream[T] = Stream()
    xs.foldLeft[Stream[T]](initialAcc) { (acc: Stream[T], el: Stream[T]) => acc ++ el }
  }

  def flattenList[T](xs: List[List[T]]): List[T] = {
    val initialAcc: List[T] = List()
    xs.foldLeft[List[T]](initialAcc) { (acc: List[T], el: List[T]) => acc ::: el }
  }

  // println(nestedStream(0))
  // println(nestedStream(0).take(1).toList)

  // println(flattenList(List(List('a'),List('b','c'),List('d','e','f'))))
  // println(flattenStream(Stream(Stream(1),Stream(2,3),Stream(4,5,6))).take(5).toList)
}
