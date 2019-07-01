object Foo extends App {
  def divisableByThreeOrFive(x: Int): Boolean = {
    (x % 3 == 0) || (x % 5 == 0)
  }

  def sumOfAllNumbersThatAre(p: (Int => Boolean))(n: Int): Int = {
    (1 to n).filter(p).fold(0) { _ + _ }
  }

  sumOfAllNumbersThatAre(divisableByThreeOrFive)(1000)
  println(sumOfAllNumbersThatAre(divisableByThreeOrFive)(1000))

  case class Palindrome(val str: String) {
    def <(that: Palindrome): Boolean = this.str.toLong < that.str.toLong
    def isValid: Boolean = str == str.reverse
    def max(that: Palindrome): Palindrome = if (this < that) that else this
  }

  def largestPalindromeProductBetween(lowerBound: Int, upperBound: Int): Palindrome = {
    // Returns all uniq integer pairs from lowerBound to upperBound
    // e.g. tuplesFrom(1, 3) => List((1,1), (1,2), (1,3), (2,2), (2,3), (3,3))
    def tuplesFrom(lowerBound: Int, upperBound: Int): List[(Int, Int)] = {
      def loop(acc: List[(Int, Int)], el: Int): List[(Int, Int)] = {
        if (el == upperBound) {
          innerTuples(el, upperBound) ::: acc
        } else {
          innerTuples(el, upperBound) ::: loop(acc, el + 1)
        }
      }

      def innerTuples(bound: Int, upperBound: Int): List[(Int, Int)] = {
        def loop(acc: List[(Int, Int)], el: Int): List[(Int, Int)] = {
          if (el == upperBound) {
            (bound, upperBound) :: acc
          } else {
            (bound, el) :: loop(acc, el + 1)
          }
        }

        loop(List(), bound)
      }

      loop(List(), lowerBound)
    }

    tuplesFrom(lowerBound, upperBound).foldLeft(new Palindrome("1")) {(acc, el: (Int, Int)) => {
      el match {
        case (x, y) => {
          var next: Palindrome = new Palindrome((x * y).toString)
          if (next.isValid) acc.max(next) else acc
        }
      }
    }}
  }

  largestPalindromeProductBetween(100, 999)

  println(largestPalindromeProductBetween(100, 999))
}
