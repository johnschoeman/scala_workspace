// Functions

// def abstraction(end: Int, f: ???): Int =
//   this match {
//     case End => end
//     case Pair(hd, tl) => f(hd, tl.abstraction(end, f))
//  }

// Function Type Declaration Syntax
// (A, B, ...) => C
// A => B

// Function Literals:
// (param: type, ...) => expression
// val sayHi = () => "Hi"
// val sum = (x: Int, y: Int) => x + y

//5.2.3.1 A better Abstraction
sealed trait IntList {

  // def fold(end: Int, f: (Int, Int) => Int): Int =
  //   this match {
  //     case End => end
  //     case Pair(hd, tl) => f(hd, tl.fold(end, f))
  //   }

  def fold[A](end: A, f: (Int, A) => A): A = {
    this match {
      case End => end
      case Pair(hd, tl) => f(hd, tl.fold(end, f))
    }
  }

  def double: IntList = {
    this.fold[IntList](End, (x: Int, y: IntList) => Pair((2 * x), y))
  }

  def sum: Int = {
    this.fold[Int](0, (x: Int, y: Int) => x + y)
  }

  def length: Int = {
    this.fold[Int](0, (x: Int, y: Int) => 1 + y)
  }

  def product: Int = {
    this.fold[Int](1, (x: Int, y: Int) => x * y)
  }
}

case object End extends IntList
case class Pair(hd: Int, tl: IntList) extends IntList

val example = Pair(4, Pair(3, Pair(2, Pair(1, End))))

assert(example.sum == 10)
assert(example.length == 4)
assert(example.product == 24)
assert(example.double == Pair(8, Pair(6, Pair(4, Pair(2, End)))))
