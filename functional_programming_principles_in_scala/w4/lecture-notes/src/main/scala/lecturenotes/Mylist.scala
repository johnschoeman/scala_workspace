package mylist

trait MyList {
  def head: Int
  def tail: MyList
}

class Cons(val head: Int, val tail: MyList) extends MyList {
  override def toString: String = "(" + head + tail + ")"
}

class Nil extends MyList {
  def head: Nothing = throw new Error("Nil.head")
  def tail: Nothing = throw new Error("Nil.tail")

  override def toString: String = "()"
}

object MyList {
  def apply(): MyList = new Nil
  def apply(x: Int): MyList = new Cons(x, new Nil)
  def apply(x: Int, y: Int): MyList = new Cons(x, new Cons(y, new Nil))
}

// class MyList(term: Int, tail: MyList) extends MyList {
//   override def toString: String = "(" + term + tail + ")"
// }

// object Nil extends MyList {
//   override def toString: String = "()"
// }
