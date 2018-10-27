// Object Literals
//
// declarations: binding names to values.
//
object Test {}

object Test2 {
  def name: String = "Test2 string"
}

// methods
object Test3 {
  def hello(name: String) =
    "Hello " + name
}

// feilds
object Test4 {
  val name = "Noel"
  def hello(other: String): String =
    name + " says hi to " + other
}

object Test7 {
  // evaluates at delcaration and returns result via subsitution
  val simpleField = {
    println("Evaluating simpleField")
    42
  }
  // evaluates at every call
  def noParameterMethod = {
    println("Evaluating noParameterMethod")
    42
  }
}

// 2.4.5.1
object Oswald {
  val name: String = "Oswald"
  val colour: String = "Black"
  val food: String = "Milk"
}

object Henderson {
  val name: String = "Henderson"
  val colour: String = "Ginger"
  val food: String = "Curry"
}

object Quentin {
  val name: String = "Quentin"
  val colour: String = "Tabby and white"
  val food: String = "Curry"
}

//2.4.5.2
object calc {
  def square(number: Double): Double = {
    println("Squaring: " + number.toString)
    number * number
  }

  def cube(number: Double): Double = {
    square(number) * number
  }
}

//2.4.5.3
object calc2 {
  def square(number: Double): Double = {
    println("Squaring: " + number.toString)
    number * number
  }

  def square(number: Int): Int = {
    println("Squaring: " + number.toString)
    number * number
  }

  def cube(number: Double): Double = {
    square(number) * number
  }

  def cube(number: Int): Int = {
    square(number) * number
  }
}

//2.4.5.4
object argh {
  def a = {
    println("a")
    1
  }

  val b = {
    println("b")
    a + 2
  }

  def c = {
    println("c")
    a
    b + "c"
  }
}

//2.4.5.5
object person {
  val firstName: String = "John"
  val lastName: String = "Person"
}
// person.type is a singleton type.
object Alien {
  def greet(p: person.type): String =
    "hello " + p.firstName + " " + p.lastName
}

//2.4.5.6
// methods are not values and are not expressions.
// calls to methods are expressions.
// functions are objects which can be invoked like methods.
object clock {
  def time = System.currentTimeMillis
}

val now = clock.time
val later = clock.time

// uniform access principle : references to fields and calls to argumentless
// methods are syntactically equivelent.
