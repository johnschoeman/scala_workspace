// Sequencing Computation

sealed trait LinkedList[A] {
  def map[B](fn: A => B): LinkedList[B] =
    this match {
      case Pair(hd, tl) => Pair(fn(hd), tl.map(fn))
      case End() => End()
    }
}
final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
final case class End[A]() extends LinkedList[A]

// sealed trait Maybe[A] {
//   def flatMap[B](fn: A => Maybe[B]): Maybe[B] = {
//     this match {
//       case Full(value) => fn(value)
//       case Empty() => Empty[B]()
//     }
//   }
// }
// final case class Full[A](value: A) extends Maybe[A]
// final case class Empty[A]() extends Maybe[A]

// A type like F[A] with a map method is called a functor. If a functor also has
// a flatMap method it is called a monad.

//5.5.4 Mapping Lists
val list: LinkedList[Int] = Pair(1, Pair(2, Pair(3, End())))
list.map((el) => el * 2)
list.map(_ * 2)
list.map((el) => el + 1)
list.map(_ + 1)
list.map((el) => el / 3)
list.map(_ / 3)

//5.5.4.2 Mapping Maybe

sealed trait Maybe[A] {
  // def map[B](fn: A => B): Maybe[B] = {
  //   this match {
  //     case Full(value) => Full(fn(value))
  //     case Empty() => Empty[B]()
  //   }
  // }
  def flatMap[B](fn: A => Maybe[B]): Maybe[B] = {
    this match {
      case Full(value) => fn(value)
      case Empty() => Empty[B]()
    }
  }
  def map[B](fn: A => B): Maybe[B] = {
    val maybeFn: A => Maybe[B] = (v) => Full(fn(v))
    this.flatMap(maybeFn)
  }
}
final case class Full[A](value: A) extends Maybe[A]
final case class Empty[A]() extends Maybe[A]

//5.5.4.3 Sequencing Computations
val list2 = List(1, 2, 3)
list2.flatMap((el) => List(el, -1 * el))

val list3: List[Maybe[Int]] = List(Full(3), Full(2), Full(1))
val list3Fn: Maybe[Int] => Maybe[Int] = (el) => {
  el match {
    case Full(value) => {
      if (value % 2 == 0) Full(value) else Empty[Int]()
    }
    case Empty() => Empty[Int]()
  }
}
list3.map(list3Fn)

//5.5.4.4 Sum
sealed trait Sum[A, B] {
  def fold[C](failure: A => C, success: B => C): C =
    this match {
      case Failure(a) => failure(a)
      case Success(b) => success(b)
    }
  def map[C](f: B => C): Sum[A, C] = {
    this match {
      case Failure(a) => Failure(a)
      case Success(b) => Success(f(b))
    }
  }
  def flatMap[C](f: B => Sum[A, C]): Sum[A, C] = {
    this match {
      case Failure(a) => Failure(a)
      case Success(b) => f(b)
    }
  }
}
final case class Failure[A, B](value: A) extends Sum[A, B]
final case class Success[A, B](value: B) extends Sum[A, B]

// val sum1: Sum[String, Int] = Failure("error")
// val sum2: Sum[String, Int] = Success(1)
val sum3: Sum[String, Int] = Success(2)
// sum1.map((value) => value.toString())
// sum2.map((value) => value.toString())

val maybeFailFn: Int => Sum[String, Int] = (value) => {
  if (value % 2 == 0) Failure("EVEN!") else Success(value)
}
// sum1.flatMap(maybeFailFn)
// sum2.flatMap(maybeFailFn)
sum3.flatMap(maybeFailFn)
