// This or That and Nothing Else: Sealed Traits

import java.util.Date

sealed trait Visitor {
  def id: String
  def createdAt: Date
  def age: Long = new Date().getTime - createdAt.getTime()
}

// final case class User(/* ... */) extends Visitor

// the majority of cases should use the sealed trait / final case class pattern
// this will warn if we miss a case in pattern matching and we can contrl extension points of sealed traits and thus make stronger guarantees about the behaviour of subtypes.

//4.2.2.1 Printing Shapes
sealed trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
  def color: Color
}

final case class Circle(radius: Double, color: Color = Red) extends Shape {
  def sides: Int = 1
  def perimeter: Double = math.Pi * 2 * radius
  def area: Double = math.Pi * radius * radius
}

final case class Rectangle(length: Double, width: Double, color: Color = Yellow) extends Shape {
  def sides: Int = 4
  def perimeter: Double = 2 * length + 2 * width
  def area: Double = length * width
}

object Draw {
  def apply(s: Shape): String = {
    s match {
      case Circle(r, color) =>
        s"A ${Draw(color)} circle of radius $r"
      case Rectangle(l, w, color) =>
        s"A ${Draw(color)} rectangle of length $l and width $w"
    }
  }
  
  def apply(color: Color): String = color match {
    case Red => "red"
    case Yellow => "yellow"
    case Pink => "pink"
    case color => if(color.isLight) "light" else "dark"
  }
}

//4.2.2.2 The Color and the Shape
sealed trait Color {
  def red: Int
  def green: Int
  def blue: Int
  def isLight: Boolean = {
    val sum: Int = red + green + blue
    sum <= 255
  }
  def isDark: Boolean = !isLight
  def name: String
}

final case class CustomColor(red: Int, green: Int, blue: Int)  extends Color {
  def name: String = 
    if (isLight) "light" else "dark"
}

final case object Red extends Color {
  val red: Int = 255
  val green: Int = 0
  val blue: Int = 0
  val name: String = "red"
}

final case object Yellow extends Color {
  val red: Int = 255
  val green: Int = 200
  val blue: Int = 200
  val name: String = "yellow"
}

final case object Pink extends Color {
  val red: Int = 255
  val green: Int = 125
  val blue: Int = 255
  val name: String = "pink"
}

//4.2.2.3 A Short Division Exercise
sealed trait DivisionResult {
}

final case class Finite(value: Int) extends DivisionResult {
}

final case object Infinite extends DivisionResult {
}

final case object divide {
  def apply(x: Int, y: Int): DivisionResult =
    if (y == 0) Infinite else Finite(x/y)
}

divide(1, 0) match {
  case Infinite => "infinite"
  case Finite(x) => s"$x"
}

