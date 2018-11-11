// Working With Data

// structural recursion:
//   variants: polymorphism(OOP) and pattern matching(FP)

// Structural Recursion using Polymorphism
// (polymorphic dispatch)

// sealed trait A {
//   def foo: String
// }
// final case class B() extends A {
//   def foo: String =
//     "It's B!"
// }
// final case class C() extends A {
//   def foo: String =
//     "It's C!"
// }
// val anA: A = B()
// anA.foo
// val anA: A = C()
// anA.foo

// the Produce Type Polymorphism Pattern
// case class A(b: B, c: C) {
//   def f: F = ???
// }
//
// the Sum Type Polymorphism Pattern
// sealed trait A {
//   def f: F
// }
// final case class B() extends A {
//   def f: F = ???
// }
// final case class C() extends A {
//   def f: F = ???
// }

// the Product Type Pattern Matching Pattern
// def f(a: A): F =
//   a match {
//     case A(b, c) => ???
//   }
// 
// the Sum Type Pattern Matching Pattern
// def f(a: A): F =
//   a match {
//     case B() => ???
//     case C() => ???
//  }

// not depending on external data and only one implementation => inside class
// with pattern maching
//

//4.5.6.1 Traffic Lights

// sealed trait TrafficLight
// case object Red extends TrafficLight
// case object Green extends TrafficLight
// case object Yellow extends TrafficLight

// Polymorphism
sealed trait PTrafficLight {
  def next: PTrafficLight
}
case object PRed extends PTrafficLight {
  def next: PTrafficLight = PGreen
}
case object PGreen extends PTrafficLight {
  def next: PTrafficLight = PYellow
}
case object PYellow extends PTrafficLight {
  def next: PTrafficLight = PRed
}

sealed trait TrafficLight {
  def next: TrafficLight =
    this match {
      case Red => Green
      case Green => Yellow
      case Yellow => Red
    }
}
case object Red extends TrafficLight
case object Green extends TrafficLight
case object Yellow extends TrafficLight

// should we implement the method inside or out side the class?
// if inside, polymorphism or pattern matching.
// a: inside with pattermatching, for better support from the complier, more of
// a state machine

//4.5.6.2 Calculation

// sealed trait Calculation
// final case class Success(result: Int) extends Calculation
// final case class Failure(reason: String) extends Calculation

sealed trait Calculation
final case class Success(result: Int) extends Calculation
final case class Failure(reason: String) extends Calculation

case object Calculator {
  def +(c: Calculation, num: Int): Calculation =
    c match {
      case Success(result) => Success(result + num)
      case Failure(reason) => Failure(reason)
    }

  def -(c: Calculation, num: Int): Calculation =
    c match {
      case Success(result) => Success(result - num)
      case Failure(reason) => Failure(reason)
    }

  def /(c: Calculation, num: Int): Calculation =
    c match {
      case Success(result) =>
        if (num == 0) Failure("Division by zero") else Success(result / num)
      case Failure(reason) => Failure(reason)
    }
}

assert(Calculator.+(Success(1), 1) == Success(2))
assert(Calculator.-(Success(1), 1) == Success(0))
assert(Calculator.+(Failure("Badness"), 1) == Failure("Badness"))
assert(Calculator./(Success(4), 2) == Success(2))
assert(Calculator./(Success(4), 0) == Failure("Division by zero"))
assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))

//4.5.6.3 Email
// use polymorphism, because dependent on outside data and multiple
// implemenations
