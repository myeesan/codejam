package qr

import org.scalatest.FunSuite
import org.scalatest.Matchers

class MinesweeperMasterMapTest extends FunSuite with Matchers {

  import qr.MinesweeperMaster2._

  test("rotate") {
    val l = List(List(0, 1), List(2, 3), List(4, 5), List(6, 7), List(8, 9))
    val expected = List(List(1, 3, 5, 7, 9), List(0, 2, 4, 6, 8))

    rotate(l) should be(expected)
  }

  test("rotate 2") {
    val l = List.tabulate(5, 10)((y, x) => y * 5 + x)
    val expected = List.tabulate(10, 5)((y, x) => x * 5 + y).reverse

    rotate(l) should be(expected)
  }
}