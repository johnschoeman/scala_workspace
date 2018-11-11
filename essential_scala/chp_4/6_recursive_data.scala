// Recursive Data

// can't define recursive data like so:
// final case class Broken(broken: Broken)

// sealed trait IntList
// case object End extends IntList
// final case class Pair(head: Int, tail: IntList) extends IntList

Pair(1, Pair(2, Pair(3, End)))

val d = End
val c = Pair(3, d)
val b = Pair(2, c)
val a = Pair(1, b)

val example = Pair(1, Pair(2, Pair(3, End)))
assert(sum(example) == 6)
assert(sum(example.tail) == 5)
assert(sum(End) == 0)

def sum(list: IntList): Int =
  list match {
    case End => 0
    case Pair(head, tail) => head + sum(tail)
  }

// Understanding the Base Case and Recursive Case
sealed trait RecursiveExample
final case class RecursiveCase(recursion: RecursiveExample)
  extends RecursiveExample
case object BaseCase extends RecursiveExample

// Tail Recursion
// tailcall:
// def method1: Int = 1
// def tailCall: Int = method1
// not tailcall:
// def notATailCall: Int = method1 + 2

import scala.annotation.tailrec

// @tailrec
// def sumntr(list: IntList): Int =
//   list match {
//     case End => 0
//     case Pair(hd, tl) => hd + sum(tl)
//   }

@tailrec
def sumtr(list: IntList, total: Int = 0): Int =
  list match {
    case End => 0
    case Pair(hd, tl) => sumtr(tl, total + hd)
  }

//4.6.3.1 A List of Methods

sealed trait IntList {
  def length: Int =
    this match {
      case End => 0
      case Pair(_, tail) => 1 + tail.length
    }

  def product: Int =
    this match {
      case End => 1
      case Pair(hd, tail) => hd * tail.product
    }

  def double: IntList =
    this match {
      case End => End
      case Pair(hd, tail) => Pair(2 * hd, tail.double)
    }
}
case object End extends IntList
final case class Pair(head: Int, tail: IntList) extends IntList

val example2 = Pair(1, Pair(2, Pair(3, End)))

assert(example.length == 3)
assert(example.tail.length == 2)
assert(End.length == 0)

assert(example.product == 6)
assert(example.tail.product == 6)
assert(End.product == 1)

assert(example.double == Pair(2, Pair(4, Pair(6, End))))
assert(example.tail.double == Pair(4, Pair(6, End)))
assert(End.double == End)

//4.6.3.2 The Forest of Trees
object TreeOps {
  def sum(tree: IntBinaryTree): Int =
    tree match {
      case Leaf => 0
      case TreePair(root, left, right) => root + sum(left) + sum(right)
    }

  def double(tree: IntBinaryTree): IntBinaryTree =
    tree match {
      case Leaf => Leaf
      case TreePair(root, left, right) =>
        TreePair(2 * root, double(left), double(right))
    }
}

sealed trait IntBinaryTree {
  def sum: Int =
    this match {
      case Leaf => 0
      case TreePair(root, left, right) => root + left.sum + right.sum
    }

  def double: IntBinaryTree =
    this match {
      case Leaf => Leaf
      case TreePair(root, left, right) => TreePair(2 * root, left.double, right.double)
    }
  def psum: Int
  def pdouble: IntBinaryTree
}
case object Leaf extends IntBinaryTree {
  def psum: Int = 0
  def pdouble: IntBinaryTree = Leaf
}
final case class TreePair(root: Int, left: IntBinaryTree, right: IntBinaryTree) extends IntBinaryTree {
  def psum: Int = root + left.psum + right.psum
  def pdouble: IntBinaryTree = TreePair(2 * root, left.pdouble, right.pdouble)
}

val tree =
  TreePair(
    2,
    TreePair(4, Leaf, Leaf),
    TreePair(
      6,
      TreePair(8, Leaf, Leaf),
      TreePair(
        10,
        TreePair(14, Leaf, Leaf),
        Leaf
        ),
      )
    )

assert(tree.left == TreePair(4, Leaf, Leaf))
assert(tree.sum == 44)
assert(tree.psum == 44)
assert(TreeOps.sum(tree) == 44)
assert(tree.left.sum == 4)
assert(tree.left.psum == 4)
assert(Leaf.psum == 0)
assert(Leaf.psum == 0)

assert(tree.left.double == TreePair(8, Leaf, Leaf))
// assert(tree.left.pdouble == TreePair(8, Leaf, Leaf))
