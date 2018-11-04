// Case Classes

case class Person(firstName: String, lastName: String) {
  def name = firstName + " " + lastName
}

// Using a case class gives us:
// a class and a companion object
val dave = new Person("Dave", "G") // a class
Person // companion object

// a field for each constructor argument
dave.firstName

// a default toString method
dave // res2: Person = Person(Dave, G)

// a sensible equals and hashCode methods
new Person("Noel", "W").equals(new Person("Noel", "Welsh"))
new Person("Noel", "W") == new Person("Noel", "Welsh")

// a copy method which creates a new object with the same field values
dave.copy()

// implementation of two traits: java.io.Serializable and scala.Product, which
// are not used directly

// The compain objects contains an apply method, which allows for omitting new.
Person("Dave", "Gurnell") == Person("Noel", "Welsh")

// The companion object also contains code to implement the extractor pattern
// for pattern matching.


// Case Objects: a case class with no constructor arguements.
// same as an object but more meaningful toString and extends Product and
// Serializable traits.
case object Citizen {
  def firstName = "John"
  def lastName = "Doe"
  def name = firstName + " " + lastName
}

Citizen.toString

//3.4.5.1 Case Cats
case class Cat(colour: String, food: String) {}

//3.4.5.2 Roger Ebert Said it Best...
case class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
  def name: String = {
    firstName + " " + lastName
  }
}

object Director {
  def older(director1: Director, director2: Director): Director =
    if (director1.yearOfBirth < director2.yearOfBirth) director1 else director2
}

case class Film(
  val name: String,
  val yearOfRelease: Int,
  val imdbRating: Double,
  val director: Director
) {
  def directorsAge: Int = {
    director.yearOfBirth
  }

  def isDirectedBy(d: Director): Boolean = {
    director.name == d.name
  }
}

object Film {
  def newer(film1: Film, film2: Film): Film = {
    if (film1.yearOfRelease < film2.yearOfRelease) film1 else film2
  }

  def highestRating(film1: Film, film2: Film): Double = {
    val film1Rating = film1.imdbRating
    val film2Rating = film2.imdbRating
    if (film1Rating > film2Rating) film1Rating else film2Rating
  }

  def oldestDirectorAtTheTime(film1: Film, film2: Film): Director = {
    if (film1.directorsAge < film2.directorsAge) film1.director else film2.director
  }
}

val mcTiernan = Director("John", "McTiernan", 1951)
val nolan = Director("Christopher", "Nolan", 1970)

val memento = Film("Memento", 2000, 8.5, nolan)
val darkKnight = Film("Dark Knight", 2008, 9.0, nolan)
val predator = Film("Predator", 1987, 7.9, mcTiernan)
val dieHard = Film("Die Hard", 1988, 8.3, mcTiernan)

//3.4.5.3 Case Class Counter
case class Counter(count: Int = 0) {
  def inc: Counter = copy(this.count + 1)
  def dec: Counter = copy(this.count - 1)
}
Counter().inc.dec == Counter().dec.inc

//3.4.5.4 Application, Application, Application
case class Person2(firstName: String, lastName: String) {
  def name = s"$firstName $lastName"
}

object Person2 {
  def apply(name: String): Person = {
    val parts = name.split(" ")
    apply(parts(0), parts(1))
  }
}
