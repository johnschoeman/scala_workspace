package daterange

class DateRange {
  def foo1 = {
    for {
      x <- 1 to 10
      y <- 1 to 10
      if x > 9 || y > 8
    } yield (y, x)
  }

  def foo3 = { 
    (1 to 10).flatMap { x =>
    for { y <- 1 to 10 } yield (y, x)
    }
  }

  def foo2 = {
    1 to 10 map(x => x)
  }

  println(foo1)
  println(foo2)
  println(foo3)
}
