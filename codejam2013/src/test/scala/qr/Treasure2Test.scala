package qr

import org.scalatest.FunSuite
import org.scalatest.Matchers
import qr.Treasure2._
import Treasure2._

class Treasure2Test extends FunSuite with Matchers {
  test("") {
    val keys = List(1, 2, 3)
    val chests = List(Chest(1, 1, Nil), Chest(2, 2, Nil), Chest(3, 3, Nil))
    val map = Map(keys, chests)

    map.solve.get should be(List(1, 2, 3))
  }

  test("test 2") {
    val keys = List(1)
    val chests = List(Chest(1, 1, List(2)), Chest(2, 2, Nil))
    val map = Map(keys, chests)

    map.solve.get should be(List(1, 2))
  }

  test("test 3") {
    val keys = List(2)
    val chests = List(Chest(1, 1, Nil), Chest(2, 2, List(1)))
    val map = Map(keys, chests)

    map.solve.get should be(List(2, 1))
  }

  test("test 4") {
    val keys = List(1)
    val chests = List(Chest(1, 1, Nil), Chest(2, 1, List(1, 2)), Chest(3, 2, Nil))
    val map = Map(keys, chests)

    map.solve.get should be(List(2, 1, 3))
  }

  test("example") {
    val keys = List(1)
    val chests = List(Chest(1, 1, Nil), Chest(2, 1, List(1, 3)), Chest(3, 2, Nil), Chest(4, 3, List(1, 2)))
    val map = Map(keys, chests)
    
    map.solve.get should be(List(2, 1, 4, 3))
  }
  
}