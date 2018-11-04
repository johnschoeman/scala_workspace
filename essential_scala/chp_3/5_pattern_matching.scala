// Pattern Matching

case class Person(firstName: String, lastName: String)

object Stormtrooper {
  def inspect(person: Person): String =
    person match {
      case Person("Luke", "Skywalker") => "Stop, rebel scum!"
      case Person("Han", "Solo") => "Stop, rebel scum!"
      case Person(first, last) => s"Move along, $first"
    }
}

// A patter can be one of:
// 1: a name, binding any value to that name
// 2: an underscore, matching any value and ignoring it
// 3: a literal, matching the value the literal denotes
// 4: a constructor-style pattern for a case class

//3.5.3.1 Feed the Cats
object ChipShop {
  def willServe(cat: Cat): Boolean =
    cat match {
      case Cat(_, "chips") => true
      case Cat(_, _) => false
    }
}

case class Cat(color: String, food: String) {}

//3.5.3.2 Get Off My Lawn!
object Dad {
  def rate(film: Film): Double =
    film match {
      case Film(_, Director("Clint Eastwood", _), _, _) => 10.0
      case Film(_, Director("John McTiernan", _), _, _) => 7.0
      case _ => 3.0
    }
}

case class Film(name: String, director: Director, imdbRating: Double, yearReleased: Int) {}
case class Director(name: String, yearOfBirth: Int) {}
