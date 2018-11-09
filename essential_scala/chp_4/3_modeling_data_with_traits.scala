// Modeling Data with Traits

// The Product Type Pattern
// A has a B and a C
// case class A(b: B, c: C)
// or
// trait A {
//   def b: B
//   def c: C
// }

// The Sum Type Pattern
// A is a B or a C
// sealed trait A
// final case class B() extends A
// final case class C() extends A

// Algebraic Data Types
// is any data that uses the above two patterns

// The Missing Patterns
// is-a and: A is a B and C
// has-a or: A has a B or C

//4.4.4.1 Stop on a Dime
sealed trait TrafficLight {
  def color: String
}

case object RedTrafficLight extends TrafficLight {
  val color: String = "red"
}
case object GreenTrafficLight extends TrafficLight {
  val color: String = "green"
}
case object YellowTrafficLight extends TrafficLight {
  val color: String = "yellow"
}

//4.4.4.2 Calculator
// a calcuation may succeed or fail
sealed trait Calculation
case class Success(result: Int) extends Calculation
case class Failure(message: String) extends Calculation

//4.4.4.3 Water, Water, Everywhere
sealed trait Source
case class Well() extends Source
case class Spring() extends Source
case class Tap() extends Source
case class Water(size: Int, source: Source, carbonated: Boolean)

