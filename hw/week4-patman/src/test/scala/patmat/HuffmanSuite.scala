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

  test("times(this is an example of a huffman tree)") {
    val input = string2Chars("this is an example of a huffman tree")
    val expected = Map(' ' -> 7, 'a' -> 4, 'e' -> 4, 'f' -> 3, 'h' -> 2, 'i' -> 2, 'm' -> 2, 'n' -> 2,
      's' -> 2, 't' -> 2, 'l' -> 1, 'o' -> 1, 'p' -> 1, 'r' -> 1, 'u' -> 1, 'x' -> 1)

    val actual = times(input)
    assert(actual.size == expected.size)
    for ((key, freq) <- actual) {
      assert(expected(key) == freq)
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton") {
    new TestTrees {
      assert(singleton(List(t1)))
      assert(singleton(List(t2)))
      assert(singleton(List(makeCodeTree(t1, t2))))
      assert(!singleton(List(t1, t2)))
    }
  }

  test("combine singleton") {
    val singleton = List(Leaf('e', 1))
    assert(combine(singleton) === singleton)
  }

  test("combine of nil") {
    assert(combine(List()) === List())
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of some leaf list 1") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 2), Leaf('x', 5), Leaf('z', 2))
    assert(combine(leaflist) === List(Leaf('z', 2), Fork(Leaf('e', 2), Leaf('t', 2), List('e', 't'), 4), Leaf('x', 5)))
  }

  test("test union") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 2), Leaf('x', 5), Leaf('z', 2))
    assert(until(singleton, combine)(leaflist) == List(Fork(
      Leaf('x', 5),
      Fork(Leaf('z', 2), Fork(Leaf('e', 2), Leaf('t', 2), List('e', 't'), 4), List('z', 'e', 't'), 6),
      List('x', 'z', 'e', 't'),
      11)
    ))
  }

  test("test create code tree") {
    val input = "tetxzxexxxz"
    assert(createCodeTree(string2Chars(input)) === Fork(
      Leaf('x', 5),
      Fork(Leaf('z', 2), Fork(Leaf('t', 2), Leaf('e', 2), List('t', 'e'), 4), List('z', 't', 'e'), 6),
      List('x', 'z', 't', 'e'),
      11)
    )
  }

  test("decoding single") {
    new CodeTest {
      assert(decode(tree, x) === "x".toList)
      assert(decode(tree, z) === "z".toList)
      assert(decode(tree, t) === "t".toList)
      assert(decode(tree, e) === "e".toList)
    }
  }
  test ("decoding multiple") {
    new CodeTest {
      assert(decode(tree, z ::: e) === "ze".toList)
      assert(decode(tree, x ::: t) === "xt".toList)
      assert(decode(tree, z ::: e ::: x) === "zex".toList)
      assert(decode(tree, z ::: e ::: t ::: x) === "zetx".toList)
    }
  }

  test("encoding single") {
    new CodeTest {
      assert(encode(tree)("x".toList) === x)
      assert(encode(tree)("z".toList) === z)
      assert(encode(tree)("t".toList) === t)
      assert(encode(tree)("e".toList) === e)
    }
  }
  test("encode multiple") {
    new CodeTest {
      assert(encode(tree)("ze".toList) === z ::: e)
      assert(encode(tree)("xt".toList) === x ::: t)
      assert(encode(tree)("zex".toList) === z ::: e ::: x)
      assert(encode(tree)("zetx".toList) === z ::: e ::: t ::: x)
    }
  }

  test("quick encoding single") {
    new CodeTest {
      assert(quickEncode(tree)("x".toList) === x)
      assert(quickEncode(tree)("z".toList) === z)
      assert(quickEncode(tree)("t".toList) === t)
      assert(quickEncode(tree)("e".toList) === e)
    }
  }
  test("quick encode multiple") {
    new CodeTest {
      assert(quickEncode(tree)("ze".toList) === z ::: e)
      assert(quickEncode(tree)("xt".toList) === x ::: t)
      assert(quickEncode(tree)("zex".toList) === z ::: e ::: x)
      assert(quickEncode(tree)("zetx".toList) === z ::: e ::: t ::: x)
    }
  }

  trait CodeTest {
    val tree = Fork(Leaf('x', 5), Fork(Leaf('z', 2), Fork(Leaf('t', 2), Leaf('e', 2), List('t', 'e'), 4), List('z', 't', 'e'), 6), List('x', 'z', 't', 'e'), 11)
    val x: List[Int] = List(0)
    val z: List[Int] = List(1, 0)
    val t: List[Int] = List(1, 1, 0)
    val e: List[Int] = List(1, 1, 1)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t1, encode(t1)("ba".toList)) === "ba".toList)
    }
  }
}
