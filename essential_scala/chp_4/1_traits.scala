// Traits

import java.util.Date

// case class Anonymous(id: String, createdAt: Date = new Date())

// case class User(id: String, email: String, createdAt: Date = new Date())

trait Visitor {
  def id: String
  def createdAt: Date

  def age: Long = new Date().getTime - createdAt.getTime
}

case class Anonymous(
  id: String,
  createdAt: Date = new Date()
) extends Visitor

case class User(
  id: String,
  email: String,
  createdAt: Date = new Date()
) extends Visitor

def older(v1: Visitor, v2: Visitor): Boolean =
  v1.createdAt.before(v2.createdAt)
older(Anonymous("1"), User("2", "test@test.com"))

//4.1.4.1 Cats, and More Cats
trait Feline {
  def colour: String
  def sound: String
}

trait BigCat extends Feline {
  override val sound = "roar"
}

case class Cat(colour: String, favouriteFood: String) extends Feline {
  def sound: String = "meow"
}

case class Lion(colour: String, maneSize: Int) extends BigCat

case class Tiger(colour: String) extends BigCat

case class Panter(colour: String) extends BigCat

//4.1.4.2 Shaping Up With Traits
trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
}

case class Circle(radius: Double) extends Shape {
  def sides: Int = 1
  def perimeter: Double = math.Pi * 2 * radius
  def area: Double = math.Pi * radius * radius
}

case class Rectangle(length: Double, width: Double) extends Shape {
  def sides: Int = 4
  def perimeter: Double = length * 2 + width * 2
  def area: Double = length * width
}

case class Square(length: Double) extends Shape {
  def sides: Int = 4
  def perimeter: Double = length * 4
  def area: Double = length * length
}

//4.1.4.3 Shaping Up 2 (Da Streets)
trait Shape2 {
  def sides: Int
  def perimeter: Double
  def area: Double
}

sealed trait Rectangular extends Shape2 {
  def length: Double
  def width: Double
  val sides: Int = 4
  def perimeter: Double = length * 2 + width * 2
  def area: Double = length * width
}

case class Circle2(radius: Double) extends Shape {
  def sides: Int = 1
  def perimeter: Double = math.Pi * 2 * radius
  def area: Double = math.Pi * radius * radius
}

case class Rectangle2(length: Double, width: Double) extends Rectangular

case class Square2(size: Double) extends Rectangular {
  val length: Double = size
  val width: Double = size
}

