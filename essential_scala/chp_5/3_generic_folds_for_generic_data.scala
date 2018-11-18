// Generic Folds for Generic Data

// sealed trait LinkedList[A]
// final case class Pair[A](head: A, tail: LinkedList[A]) extends
//   LinkedList[A]
// final case class End[A]() extends LinkedList[A]
// def fold[A](end: A, f: (Int, A) => A): A =
//   this match {
//     case End => end
//     case Pair(hd, tl) => f(hd, tl.fold(end, f))
//   }

// sealed trait LinkedList[A] {
//   def fold[B](end: B, f: (A, B) => B): B =
//     this match {
//       case End() => end
//       case Pair(hd, tl) => f(hd, tl.fold(end, f))
//     }
// }
// final case class Pair[A](head: A, tail: LinkedList[A]) extends
//   LinkedList[A]
// final case class End[A]() LinkedList[A]

// any function you care to write can be written in terms of fold.

// Placeholder Syntax
// ((_: Int) * 2)
// _ + _
// foo(_)
// foo(_, b)
// _(foo)

// Converting methods to functions

object Sum {
  def sum(x: Int, y: Int) = x + y
}
// Sum.sum
(Sum.sum _)
object MathStuff {
  def add1(num: Int) = num + 1
}
// Counter(2).adjust(MathStuff.add1)

// Multiple Parameter Lists

def example(x: Int)(y: Int) = x + y
example(1)(2)

// def fold[B](end: B)(pair: (A, B) => B): B =
//   this match {
//     case End() => end
//     case Pair(hd, tl) => pair(hd, tl.fold(end, pair))
//   }
// fold(0){ (total, elt) => total + elt }

// 5.3.4.1 Tree
sealed trait Tree[A] {
  def fold[B](leaf: A => B, node: (B, B) => B): B =
    this match {
      case Leaf(node) => leaf(node)
      case Node(left, right) => node(left.fold(leaf, node), right.fold(leaf, node))
    }
}
final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
}
final case class Leaf[A](node: A) extends Tree[A] {
}

var tree1: Tree[Int] = Node[Int](Leaf[Int](2), Leaf[Int](3))
var tree2: Tree[Int] = Node[Int](tree1, Leaf[Int](5))
var tree3: Tree[String] = Node(Node(Leaf("c"), Leaf("d")), Leaf("e"))

object TreeStuff {
  def concatLeaf(x: String): String = x
  def concatNode(x: String, y: String): String = s"$x $y".trim
  def sumLeaf(x: Int): Int = x
  def sumNode(x: Int, y: Int): Int = x + y
}

assert(tree3.fold(TreeStuff.concatLeaf, TreeStuff.concatNode) == "c d e")
assert(tree1.fold(TreeStuff.sumLeaf, TreeStuff.sumNode) == 5)
assert(tree2.fold(TreeStuff.sumLeaf, TreeStuff.sumNode) == 10)

val tree: Tree[String] =
  Node(Node(Leaf("To"), Leaf("iterate")),
       Node(Node(Leaf("is"), Leaf("human,")),
            Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))))

assert(tree.fold(TreeStuff.concatLeaf, TreeStuff.concatNode) == "To iterate is human, to recurse divine")
