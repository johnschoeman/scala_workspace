package streamnotes

class StreamNotes {
  def streamRange(lo: Int, hi: Int): Stream[Int] = {
    print(lo+" ")
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))
  }

  // println(streamRange(1, 10).take(8).toList)

  def from(n: Int): Stream[Int] = n #:: from(n + 1)

  // println(from(10).take(3).toList)

  val nats = from(0)
  val m4s = nats map (_ * 4)

  println(m4s take 10 toList)

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0))

  val primes = sieve(from(2))
  println(primes.take(100).toList)

  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }

  def isGoodEnough(guess: Double, x: Double) =
    math.abs((guess * guess - x) / x) < 0.0001

  println(sqrtStream(4).take(10).toList)

  println(sqrtStream(4).filter(isGoodEnough(_, 4)).take(10).toList)
}
