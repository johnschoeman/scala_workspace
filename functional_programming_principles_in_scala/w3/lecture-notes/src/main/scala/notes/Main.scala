import notes.{ClassHierachies}
import wtf.{Cons, Nil}

object Main extends App {
  val l1 = new Cons[Int](1, new Cons[Int](2, new Cons[Int](3, new Nil[Int])))
  val l2 = new Cons[String]("1", new Cons[String]("foo", new Cons[String]("3", new Nil[String])))
  println(l2.nth(1))
}

class Set {
  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 5 incl 1 incl 9
  val t3 = new NonEmpty(4, Empty, Empty)
  val t4 = t3 incl 2 incl 6 incl 0
  println(t4.union(t2))

  val x = new ClassHierachies
  println(x.test)

  abstract class IntSet {
    def contains(x: Int): Boolean
    def incl(x: Int): IntSet
    def union(other: IntSet): IntSet
  }

  // class Empty extends IntSet {
  object Empty extends IntSet {
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
    def union(other: IntSet) = other
    override def toString = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    def union(other: IntSet): IntSet = {
      other union right union left incl elem
    }


    override def toString = "{" + left + elem + right + "}"
  }
}
