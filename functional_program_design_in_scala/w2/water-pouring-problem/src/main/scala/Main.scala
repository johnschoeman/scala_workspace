import pouring._

object Main extends App {
  val pour = new Pouring(Vector(4,9))
  println(pour.solutions(6).take(1).toList)
}
