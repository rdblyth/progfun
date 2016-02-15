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

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
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

  test("times returns the number of times a character occurs in a List") {
    val frequency = times(List('a','b', 'a'))
    assert(frequency.length == 2)
    assert(frequency.contains(('a', 2)))
    assert(frequency.contains(('b', 1)))

  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton returns true if a List[Tree] contains a single tree") {
    assert(singleton(List(Leaf('e', 1))) == true)
  }

  test("singleton returns false if a List[Tree] contains more than one tree") {
    assert(singleton(List(Leaf('e', 1), Leaf('t', 2))) == false)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine with List less than two elements returns List unchanged") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) == leaflist)
  }

  test("until should combine trees and return a single tree") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val singleTree = until(singleton, combine)(leaflist)
    assert(singleton((singleTree)))

    singleTree(0) match {
      case Fork(left, right, c, weight) => {
        assert(c == List('e','t', 'x'))
        assert(weight == 7)
        assert(chars(left) == List('e','t'))
        assert(chars(right) == List('x'))
      }
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
