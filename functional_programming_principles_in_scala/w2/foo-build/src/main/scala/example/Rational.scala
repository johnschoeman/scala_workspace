package example

object Main extends App {
  val x = new Rational(3, 2)
  val y = new Rational(2, 3)
  val z = new Rational(7, 9)

  println(x + y)
  println(x - y - z)
  println(-x)
  println(new Rational(2))

  class Rational(x: Int, y: Int) {
    require(y > 0, "denom must be nonzero")

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    private val g = gcd(x, y)
    val numer = x
    val denom = y

    def this(x: Int) = this(x, 1)

    def less(that: Rational) = numer * that.denom < that.numer * numer

    def max(that: Rational) = {
      if (this.less(that)) that else this
    }

    def +(that: Rational) = {
      new Rational(
        numer * that.denom + denom * that.numer,
        denom * that.denom
        )
    }

    def unary_- : Rational = {
      new Rational(-numer, denom)
    }

    def -(that: Rational) = this + -that

    override def toString = {
      numer / g + "/" + denom / g
    }
  }
}
