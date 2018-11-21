// Modeling Data with Generic Types
//
// Generic Product Types
// def intAndString: ??? = // ...
// def booleanAndDouble: ??? = // ...
// def intAndString: Pair[Int, String] = // ...
// def booleanAndDouble: Pair[Boolean, Double] = // ...

//5.4.1.1 Exercise: Pairs
case class Pair[A, B](one: A, two: B) {
}

val pair = Pair[String, Int]("hi", 2)
val otherPair = Pair(1.0, true)

// Tuples
//
// Tuple2("hi", 1)
// ("hi", 1)
// ("hi", 1, true)
// def tuplized[A, B](in: (A, B)) = in._1
// tuplized(("A", 1))
// (1, "a") match {
  // case (a, b) => a + b
// }
// val x = (1, "b", true)
// x._1
// x._3

// Generic Sum Types
//
// def intOrSTring(input: Boolean) =
//   if(input == true) 123 else "abc"
// intOrString: (input: Boolean)Any
// def intOrString(input: Boolean): Sum[Int, String] =
  // if(input == true) {
    // Left[Int, String](123)
  // } else {
    // Right[Int, String]("abc")
  // }
// intOrString: (input: Boolean)sum.Sum[Int,String]

//5.4.3.1 Exercise: Generic Sum Type

// trait Sum[A, B]
// case class Left[A, B](value: A) extends Sum[A, B]
// case class Right[A, B](value: B) extends Sum[A, B]

def intOrString(input: Boolean): Sum[Int, String] =
  if(input == true) {
    Left[Int, String](123)
  } else {
    Right[Int, String]("abc")
  }
Left[Int, String](1).value
Right[Int, String]("foo").value
val sum: Sum[Int, String] = Right("foo")
sum match {
  case Left(x) => x.toString
  case Right(x) => x
}

// Generic Optional Values
//
//5.4.4.1 Exercise: Maybe that Was a Mistake

// trait Maybe[A]
// case class Full[A](value: A) extends Maybe[A]
// case class Empty[A]() extends Maybe[A]

val perhaps1: Maybe[Int] = Empty[Int]
val perhaps2: Maybe[Int] = Full(1)

//5.4.6.1 Generics versus Traits
trait Maybe[A] {
  def fold[B](empty: B, full: A => B): B = {
    this match {
      case Empty() => empty
      case Full(value) => full(value)
    }
  }
}
case class Full[A](value: A) extends Maybe[A]
case class Empty[A]() extends Maybe[A]

//5.4.6.3 Folding Sum
trait Sum[A, B] {
  def fold[C](left: A => C, right: B => C): C = {
    this match {
      case Left(value) => left(value)
      case Right(value) => right(value)
    }
  }
}
case class Left[A, B](value: A) extends Sum[A, B]
case class Right[A, B](value: B) extends Sum[A, B]

val left = Left[Int, Double](3)
val right = Right[Int, Double](3.0)
left.fold[Boolean]((a) => a == 3 , (a) => a == 3.0 )
right.fold[Boolean]((a) => a == 5 , (a) => a == 4.0 )
