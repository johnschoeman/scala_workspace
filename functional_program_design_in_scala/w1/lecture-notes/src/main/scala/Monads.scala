package monads

class Monads {
  val f = (x: Int) => x + 1
  // val unit = (x) => x
  val unit = (x: Int) => List(x)

  val m = List(1,2,3)

  // println(m flatMap ( x => unit(f(x))))
  // println(m flatMap (f andThen unit))
}
