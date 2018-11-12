// Generics

final case class Box[A](value: A)

// the syntax [A] is called a type parameter

def generic[A](in: A): A = in

// Invariant Generic Sum Type Pattern:
//
// sealed trait Calculation
// final case class Success(result: Double) extends Calculation
// final case class Failure(reason: String) extends Calculation
//
// becomes...

sealed trait Result[A]
case class Success[A](result: A) extends Result[A]
case class Failure[A](reason: String) extends Result[A]

//5.1.3.1 Generic List
// sealed trait IntList
// case object End extends IntList
// final case class Pair(head: Int, tail: IntList) extends IntList
sealed trait LinkedList[A] {
  def length: Int = {
    this match {
      case Pair(head, tail) => 1 + tail.length
      case End() => 0
    }
  }

  def contains(item: A): Boolean = {
    this match {
      case Pair(head, tail) =>
        if (head == item) {
          true
        } else if (tail.contains(item)) {
          true
        } else {
          false
        }
      case End() => false
    }
  }

  def apply(index: Int): Result[A] = {
    this match {
      case Pair(head, tail) =>
        if (index == 0) {
          Success(head)
        } else {
          tail.apply(index - 1)
        }
      case End() =>
        Failure("Index out of bounds")
    }
  }
}
case class End[A]() extends LinkedList[A]
case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

val example = Pair(1, Pair(2, Pair(3, End())))

assert(example.length == 3)
assert(example.tail.length == 2)
assert(End().length == 0)

assert(example.contains(3) == true)
assert(example.contains(4) == false)
assert(End().contains(0) == false)
// example.contains("not an Int")

// assert(example(0) == 1)
// assert(example(1) == 2)
// assert(example(2) == 3)
// assert(try {
//   example(3)
//   false
// } catch {
//   case e: Exception => true
// })

assert(example(0) == Success(1))
assert(example(1) == Success(2))
assert(example(2) == Success(3))
assert(example(3) == Failure("Index out of bounds"))

