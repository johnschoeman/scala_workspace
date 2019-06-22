package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  trait TestLists {
    val l1 = List('a', 'b', 'c')
    val l2 = List('c', 'd', 'c', 'a', 'c')
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    new TestLists {
      assert(times(l1) == List(('a', 1), ('b', 1), ('c', 1)))
      assert(times(l2) == List(('c', 3), ('d', 1), ('a', 1)))
    }
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
    val leaflist2 = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(combine(leaflist2) === List(Leaf('x', 4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5)))
  }

  test("until") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7)))
  }

  test("createCodeTree") {
    val chars = List('a', 'a', 'a', 'b', 'b', 'c')
    assert(createCodeTree(chars) === Fork(Fork(Leaf('c',1),Leaf('b',2),List('c', 'b'),3),Leaf('a',3),List('c', 'b', 'a'),6))
  }

  test("decode") {
    new TestTrees {
      val message1 = List(0,1,1,1,0)
      assert(decode(t1, message1) === "abbba".toList)
      val message2 = List(0,0,0,1,1,1,0,0,0,1,0,1,1,1)
      assert(decode(t2, message2) === "abddabbdd".toList)
    }
  }

  test("encode") {
    new TestTrees {
      val text1: List[Char] = List('a','b','b','b','a')
      assert(encode(t1)(text1) === List(0,1,1,1,0))
      val text2: List[Char] = List('a','b','d','d','a','b','b','d','d')
      assert(encode(t2)(text2) === List(0,0,0,1,1,1,0,0,0,1,0,1,1,1))
    }
  }

  test("codeBits") {
    val table: CodeTable = List(('c', List(0,1)), ('a', List(0,1,0,1)), ('b', List(1,1,1,1)))
    assert(codeBits(table)('a') === List(0,1,0,1))
    assert(codeBits(table)('b') === List(1,1,1,1))
  }

  test("convert") {
    val t1: CodeTree = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 4)
    assert(convert(t1) === List(('a', List(0)), ('b', List(1))))
    val t2: CodeTree = Fork(Fork(Leaf('c',1),Leaf('b',2),List('c', 'b'),3),Leaf('a',3),List('c', 'b', 'a'),6)
    assert(convert(t2) === List(('c', List(0, 0)), ('b', List(0, 1)), ('a', List(1))))
  }

  test("quickEncode") {
    new TestTrees {
      assert(quickEncode(t1)("abbba".toList) === List(0,1,1,1,0))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
