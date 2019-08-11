import org.scalatest.FunSuite

import pouring._

class PouringSuite extends FunSuite  {
  test("true is true") {
    // assert(Pouring(Vector(4,9)).sovleFor(6) == List())
  }

  test("Path endState returns the endState") {
    val pour = Pouring(Vector(4,9))
    val path = pour.Path(List(
        pour.Pour(1,0),
        pour.Fill(1),
        pour.Pour(1,0),
        pour.Empty(0),
        pour.Pour(1,0),
        pour.Empty(0),
        pour.Pour(1,0),
        pour.Fill(1),
        pour.Empty(0)
      ))
    assert(path.endState == Vector(4,6))
  }

  test("Comes up with a correct solution") {
    val pour = Pouring(Vector(4,9))
    val solution = pour.solveFor(6)
    val expected =
      pour.Path(List(
        pour.Pour(1,0),
        pour.Fill(1),
        pour.Pour(1,0),
        pour.Empty(0),
        pour.Pour(1,0),
        pour.Empty(0),
        pour.Pour(1,0),
        pour.Fill(1)
      ))

    assert(solution == expected)
  }
}
