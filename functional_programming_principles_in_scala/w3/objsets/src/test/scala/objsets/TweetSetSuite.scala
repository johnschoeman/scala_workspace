package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
<<<<<<< HEAD
=======
    val set6 = set1.incl(new Tweet("b", "b body 0", 0))
    val set7 = set6.incl(new Tweet("a", "a body 1", 2))
    val set8 = set7.incl(new Tweet("c", "a body 2", 4))
    val set9 = set8.incl(new Tweet("d", "a body 3", 3))
    val set10 = set9.incl(new Tweet("e", "a body 4", 1))
    val set11 = set10.incl(new Tweet("e", "a body 5", 4))
>>>>>>> Final state
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

<<<<<<< HEAD
=======
  test("filter: a on set10") {
    new TestSets {
      assert(size(set10.filter(tw => tw.user == "a")) === 1)
    }
  }

>>>>>>> Final state
  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

<<<<<<< HEAD
=======
  test("filter: 4") {
    new TestSets {
      assert(size(set10.filter(tw => tw.retweets == 4)) === 1)
    }
  }

  test("filter: 5") {
    new TestSets {
      assert(size(set11.filter(tw => tw.retweets == 4)) === 2)
    }
  }

>>>>>>> Final state
  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

<<<<<<< HEAD
=======
  test("mostRetweets") {
    new TestSets {
      println(set10.mostRetweeted)
      assert(set10.mostRetweeted.retweets == 4)
    }
  }

>>>>>>> Final state
  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

<<<<<<< HEAD
=======
  test("descending: set10") {
    new TestSets {
      val trends = set10.descendingByRetweet
      assert(!trends.isEmpty)
      println(trends)
      assert(trends.head.user == "c" && trends.head.retweets == 4)
    }
  }

>>>>>>> Final state
  }
