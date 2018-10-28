// Classes

class Person {
  val firstName = "Noel"
  val lastName = "Welsh"
  def name = firstName + " " + lastName
}

// classes are not expressions an a class is not a value
val noel = new Person
noel.firstName
noel

val newNoel = new Person
val anotherNewNoel = new Person

object alien {
  def greet(p: Person) =
    "Greetings, " + p.firstName + " " + p.lastName
}

alien.greet(noel)
alien.greet(newNoel)

// Constructors
class Person2(first: String, last: String) {
  val firstName = first
  val lastName = last
  def name = firstName + " " + lastName
}

val dave = new Person2("Dave", "Gurnell")
dave.name

class Person3(val firstName: String, val lastName: String) {
  def name = firstName + " " + lastName
}

new Person3("Dave", "Person3").firstName

// Default and Keyword Parameters
val newPerson = new Person3(lastName = "Last", firstName = "First")

def greet(firstName: String = "Some", lastName: String = "Body") =
  "Greetings, " + firstName + " " + lastName + "!"

// Scala's Type Hierarchy
def badness = throw new Exception("Error")
// badness: Nothing
def otherbadness = null
// otherbadness: Null

//3.1.6.1
class Cat(val name: String, val color: String, val food: String) {}
val oswald = new Cat("Oswald", "Black", "Milk")
val henderson = new Cat(name = "Henderson", color = "Ginger", food = "Chips")
val quentin = new Cat(color = "Tabby and White", food = "Curry", name = "Quentin")

//3.1.6.2
class ChipShop {
  def willServe(c: Cat): Boolean =
    if (c.food == "Chips")
      true
    else
      false
}

//3.1.6.3
class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
  def name: String = {
    firstName + " " + lastName
  }

  def copy(
    firstName: String = this.firstName,
    lastName: String = this.lastName,
    yearOfBirth: Int = this.yearOfBirth): Director =
      new Director(firstName, lastName, yearOfBirth)
}

class Film(
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

  def copy(
    name: String = this.name,
    yearOfRelease: Int = this.yearOfRelease,
    imdbRating: Double = this.imdbRating,
    director: Director = this.director): Film =
    new Film(name, yearOfRelease, imdbRating, director)
}

val eastwood = new Director("Clint", "Eastwood", 1930)
val mcTiernan = new Director("John", "McTiernan", 1951)
val nolan = new Director("Christopher", "Nolan", 1970)
val someBody = new Director("Just", "Some Body", 1990)

val memento = new Film("Memento", 2000, 8.5, nolan)
val darkKnight = new Film("Dark Knight", 2008, 9.0, nolan)
val inception = new Film("Inception", 2010, 8.8, nolan)
val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7, eastwood)
val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9, eastwood)
val unforgiven = new Film("Unforgiven", 1992, 8.3, eastwood)
val granTorino = new Film("Gran Torino", 2008, 8.2, eastwood)
val invictus = new Film("Invictus", 2009, 7.4, eastwood)

val predator = new Film("Predator", 1987, 7.9, mcTiernan)
val dieHard = new Film("Die Hard", 1988, 8.3, mcTiernan)
val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6, mcTiernan)
val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8, mcTiernan)

//3.1.6.4
class Counter(val count: Int) {
  def inc: Counter =
    new Counter(this.count + 1)

  def dec: Counter =
    new Counter(this.count - 1)
}
// question: when to use this and when to not use this, what is the this
// keyword?

//3.1.6.5 - Counting Faster
class FastCounter(val count: Int) {
  def inc: FastCounter = inc()
  def dec: FastCounter = dec()
  def inc(amount: Int = 1): FastCounter =
    new FastCounter(this.count + amount)

  def dec(amount: Int = 1): FastCounter =
    new FastCounter(this.count - amount)
}

//3.1.6.6 - Additional Counting
class Adder(amount: Int) {
  def add(in: Int) = in + amount
}

class AdderCounter(val count: Int) {
  def inc(amount: Int = 1): AdderCounter =
    new AdderCounter(this.count + amount)

  def dec(amount: Int = 1): AdderCounter =
    new AdderCounter(this.count - amount)

  def adjust(adder: Adder) =
    new AdderCounter(adder.add(this.count))
}

