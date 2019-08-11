import pouring._

object Main extends App {
  val pour = new Pouring(Vector(4,9))
  println(pour.solveFor(6))
}
