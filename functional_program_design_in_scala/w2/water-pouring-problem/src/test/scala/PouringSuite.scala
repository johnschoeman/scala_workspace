import org.scalatest.FunSuite

import pouring._

class PouringSuite extends FunSuite  {
  test("true is true") {
    // assert(Pouring(Vector(4,9)).sovleFor(6) == List())
  }

  test("Comes up with a correct solution") {
    val pour = Pouring(Vector(4,9))
    val solution = pour.solveFor(6)
    val expected =
      pour.Path(List(
        (pour.Pour(1,0),Vector(4, 6)),
        (pour.Fill(1),Vector(1, 9)),
        (pour.Pour(1,0),Vector(1, 0)),
        (pour.Empty(0),Vector(0, 1)),
        (pour.Pour(1,0),Vector(4, 1)),
        (pour.Empty(0),Vector(0, 5)),
        (pour.Pour(1,0),Vector(4, 5)),
        (pour.Fill(1),Vector(0, 9)),
        (pour.Empty(0),Vector(0, 0))
      ))

    assert(solution == expected)
  }
}
