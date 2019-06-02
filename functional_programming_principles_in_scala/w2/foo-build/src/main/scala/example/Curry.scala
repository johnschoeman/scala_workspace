package example

object Curry {
  // def sum(f: Int => Int)(a: Int, b: Int): Int = {
  //   def loop(a: Int, acc: Int): Int = {
  //     if (a > b) acc
  //     else loop(a + 1, f(a) + acc)
  //   }
  //   loop(a, 0)
  // }

  // def product(f: Int => Int)(a: Int, b: Int): Int = {
  //   def loop(a: Int, acc: Int): Int = {
  //     if (a > b) acc
  //     else loop(a + 1, f(a) * acc)
  //   }
  //   loop(a, 1)
  // }

  def factorial(a: Int): Int = {
    product((a: Int) => a)(1, a)
  }

  def gen(f: Int => Int)(zero: Int)(a: Int, b: Int)(combine: Int => (Int => Int)): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, combine(acc)(a))
    }
    loop(a, zero)
  }

  def mult(f: Int => Int)(acc: Int)(a: Int): Int = {
    f(a) * acc
  }

  def add(f: Int => Int)(acc: Int)(a: Int): Int = {
    f(a) + acc
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    gen(f)(1)(a,b)(mult(f))
  }

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    gen(f)(0)(a,b)(add(f))
  }

    println(product((a: Int) => a)(1, 4))
    println(sum((a: Int) => a)(1, 4))
    // assert(product((a: Int) => a*a)(1, 3) == 36)
    println(product((a: Int) => a*a)(1, 3))
    println(factorial(5))
}

