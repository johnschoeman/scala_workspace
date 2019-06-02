package example

object Hello {
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }
    assert(sum((a: Int) => a, 1, 3) == 6)
    assert(sum((a: Int) => a*a, 1, 3) == 14)
    println(sum((a: Int) => a*a, 1, 3))
}

