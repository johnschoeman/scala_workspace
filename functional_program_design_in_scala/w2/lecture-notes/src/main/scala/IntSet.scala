package intset

class IntSetTest {
  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
  }

  object Empty extends IntSet {
    def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
    def contains(x: Int): Boolean = false
    def union(other: IntSet): IntSet = other
    override def toString: String = ""
  }

  case class NonEmpty(x: Int, l: IntSet, r: IntSet) extends IntSet {
    def incl(y: Int): IntSet = {
      if (y < x) new NonEmpty(x, l incl y, r)
      else if (x < y) new NonEmpty(x, l, r incl y)
      else this
    }

    def contains(y: Int): Boolean = {
      if (x < y) l contains y
      else if (x > y) r contains y
      else true
    }

    def union(other: IntSet): IntSet = {
      (l union (r union other)) incl x
    }

    override def toString: String = {
      "(" + x + l + r + ")"
    }
  }

  val a = NonEmpty(2, Empty, Empty)
  val b = a.incl(1).incl(2)
  println(b.contains(3))
  println(b)
}
