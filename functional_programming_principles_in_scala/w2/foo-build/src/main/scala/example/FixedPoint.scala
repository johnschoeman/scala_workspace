package example

object FixedPoint {
  val tolerance = 0.001

  def isCloseEnough(x: Double, y: Double): Boolean = {
    // (abs((x - y) / x))) / x < tolerance
    Math.abs((x - y) / x) / x < tolerance
  }

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
    def iterate(guess: Double): Double = {
      // val next = (f(guess) + guess) / 2
      val next = f(guess)

      if (isCloseEnough(next, guess)) { next }
      else { iterate(next) }
    }
    iterate(firstGuess)
  }

  def sqrt(x: Double): Double = {
    fixedPoint(averageDamp(y => x/y))(1.0)
  }

  // println(fixedPoint((x: Double) => (1 + (x / 2)))(1))
  println(sqrt(2.0))
}
