// Companion Object

class Timestamp(val seconds: Long)

object Timestamp {
  def apply(hours: Int, minutes: Int, seconds: Int): Timestamp =
    new Timestamp(hours*60*60 + minutes*60 + seconds)
}

Timestamp(1, 1, 1).seconds

//3.3.2.1 Friendly Person Factory
class Person(val firstName: String, val lastName: String) {
  def name: String =
    s"$firstName $lastName"
}

object Person {
  def apply(name: String) = {
    val parts = name.split(" ")
    new Person(parts(0), parts(1))
  }
}

Person("John Doe")

//3.3.2.2 Extended Body of Work
class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
  def name: String = {
    s"$firstName $lastName"
  }

  def copy(
    firstName: String = this.firstName,
    lastName: String = this.lastName,
    yearOfBirth: Int = this.yearOfBirth): Director =
      new Director(firstName, lastName, yearOfBirth)
}

object Director {
  def apply(firstName: String, lastName: String, yearOfBirth: Int) =
    new Director(firstName, lastName, yearOfBirth)

  def older(d1: Director, d2: Director): Director =
    if(d1.yearOfBirth < d2.yearOfBirth) d1 else d2
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

object Film {
  def apply(name: String, yearOfRelease: Int, imdbRating: Double, director: Director): Film =
    new Film(name, yearOfRelease, imdbRating, director)

  def newer(film1: Film, film2: Film): Film =
    if (film1.yearOfRelease < film2.yearOfRelease) film1 else film2

  def highestRating(f1: Film, f2: Film): Double = {
    val rating1 = f1.imdbRating
    val rating2 = f2.imdbRating
    if(rating1 > rating2) rating1 else rating2
  }

  def oldestDirectorAtTheTime(f1: Film, f2: Film): Director =
    if(Director.older(f1.director, f2.director) == f1.director) f1.director else f2.director
}

//3.3.2.3 Type or Value?
val prestige: Film = bestFilmByChristopherNolan()
//type
new Film("Last Action Hero", 1993, mcTiernan)
// type
Film("Last Action Hero", 1993, mcTiernan)
//singleton object
Film.newer(highPlainsDrifter, thomasCrownAffair)
//singleton object
Film.type
//singleton object (note that the whole of 'Film.type' is a type)
