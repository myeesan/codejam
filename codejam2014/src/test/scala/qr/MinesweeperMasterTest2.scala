package qr

import org.scalatest.FunSuite
import org.scalatest.Matchers

import qr.MinesweeperMaster2._

// M: 마인의 개수
// 0 ≤ M < R * C

class MinesweeperMasterTest2 extends FunSuite with Matchers {
  test("test base method") {
    val game = lineToGame("5 5 7")

    game.mine should be(5)
    game.cellCount should be(35)
    game.emptyCount should be(30)
    game.minWidth should be(5)
  }

  test("1 x 1 with 1 mine") {
    val game = lineToGame("1 1 1")

    game.isPossible should be(false)
    game.getMap should be(None)
  }

  test("1 x 1 with no mine") {
    val game = lineToGame("0 1 1")

    game.isPossible should be(true)
    game.getMap.get should be(MineMap(0, 1, 1))
  }

  test("2 x 1") {
    // 0 mine
    lineToGame("0 1 2").isPossible should be(true)
    // 1 mine
    lineToGame("1 1 2").isPossible should be(true)
    // 2 mine
    lineToGame("2 2 1").isPossible should be(false)
  }

  test("3 x 1, 4 x 1") {
    // 0 mine
    lineToGame("0 1 3").isPossible should be(true)
    // 1 mine
    lineToGame("1 1 3").isPossible should be(true)
    // 2 mine
    lineToGame("2 1 3").isPossible should be(true)
    // 3 mine
    lineToGame("3 1 3").isPossible should be(false)

    // 0 mine
    lineToGame("0 1 4").isPossible should be(true)
    // 1 mine
    lineToGame("1 1 4").isPossible should be(true)
    // 2 mine
    lineToGame("2 1 4").isPossible should be(true)
    // 3 mine
    lineToGame("3 1 4").isPossible should be(true)
    // 4 mine
    lineToGame("4 1 4").isPossible should be(false)
  }

  test("2 x 2") {
    // 0 mine
    lineToGame("0 2 2").isPossible should be(true)
    // 1 mine
    lineToGame("1 2 2").isPossible should be(false)
    // 2 mine
    lineToGame("2 2 2").isPossible should be(false)
    // 3 mine
    lineToGame("3 2 2").isPossible should be(true)
    // 4 mine
    lineToGame("4 2 2").isPossible should be(false)
  }

  test("2 x 3, 2 x 4") {
    // 0 mine
    lineToGame("0 2 3").isPossible should be(true)
    // 1 mine
    lineToGame("1 2 3").isPossible should be(false)
    // 2 mine
    lineToGame("2 2 3").isPossible should be(true)
    // 3 mine
    lineToGame("3 2 3").isPossible should be(false)
    // 4 mine
    lineToGame("4 2 3").isPossible should be(false)
    // 5 mine
    lineToGame("5 2 3").isPossible should be(true)
    // 6 mine
    lineToGame("6 2 3").isPossible should be(false)

    // 0 mine
    lineToGame("0 2 4").isPossible should be(true)
    // 1 mine
    lineToGame("1 2 4").isPossible should be(false)
    // 2 mine
    lineToGame("2 2 4").isPossible should be(true)
    // 3 mine
    lineToGame("3 2 4").isPossible should be(false)
    // 4 mine
    lineToGame("4 2 4").isPossible should be(true)
    // 5 mine
    lineToGame("5 2 4").isPossible should be(false)
    // 6 mine
    lineToGame("6 2 4").isPossible should be(false)
    // 7 mine
    lineToGame("7 2 4").isPossible should be(true)
    // 8 mine
    lineToGame("8 2 4").isPossible should be(false)
  }

  test("3 x 3, 3 x 4, 3 x 5") {
    // 0 mine
    lineToGame("0 3 3").isPossible should be(true)
    // 1 mine
    lineToGame("1 3 3").isPossible should be(true)
    // 2 mine
    lineToGame("2 3 3").isPossible should be(false)
    // 3 mine
    lineToGame("3 3 3").isPossible should be(true)
    // 4 mine
    lineToGame("4 3 3").isPossible should be(false)
    // 5 mine
    lineToGame("5 3 3").isPossible should be(false)
    // 6 mine
    lineToGame("6 3 3").isPossible should be(false)
    // 7 mine
    lineToGame("7 3 3").isPossible should be(false)
    // 8 mine
    lineToGame("8 3 3").isPossible should be(true)
    // 9 mine
    lineToGame("9 3 3").isPossible should be(false)

    // 0 mine
    lineToGame("0 3 4").isPossible should be(true)
    // 1 mine
    lineToGame("1 3 4").isPossible should be(true)
    // 2 mine
    lineToGame("2 3 4").isPossible should be(true)
    // 3 mine
    lineToGame("3 3 4").isPossible should be(true)
    // 4 mine
    lineToGame("4 3 4").isPossible should be(true)
    // 5 mine
    lineToGame("5 3 4").isPossible should be(false)
    // 6 mine
    lineToGame("6 3 4").isPossible should be(true)
    // 7 mine
    lineToGame("7 3 4").isPossible should be(false)
    // 8 mine
    lineToGame("8 3 4").isPossible should be(false)
    // 9 mine
    lineToGame("9 3 4").isPossible should be(false)
    // 10 mine
    lineToGame("10 3 4").isPossible should be(false)
    // 11 mine
    lineToGame("11 3 4").isPossible should be(true)
    // 12 mine
    lineToGame("12 3 4").isPossible should be(false)

    // 6 mine
    lineToGame("6 3 5").isPossible should be(true)
    // 7 mine
    lineToGame("7 3 5").isPossible should be(true)
    // 8 mine
    lineToGame("8 3 5").isPossible should be(false)
    // 9 mine
    lineToGame("9 3 5").isPossible should be(true)
    // 10 mine
    lineToGame("10 3 5").isPossible should be(false)
    // 11 mine
    lineToGame("11 3 5").isPossible should be(false)
    // 12 mine
    lineToGame("12 3 5").isPossible should be(false)
    // 13 mine
    lineToGame("13 3 5").isPossible should be(false)
    // 14 mine
    lineToGame("14 3 5").isPossible should be(true)
    // 15 mine
    lineToGame("15 3 5").isPossible should be(false)
  }

  // minLine이 4 이상인 경우에는 8개 이상이면 무조건 가능함
  test("4 x 4") {
    // 0 mine
    lineToGame("0 4 4").isPossible should be(true)
    // 1 mine
    lineToGame("1 4 4").isPossible should be(true)
    // 2 mine
    lineToGame("2 4 4").isPossible should be(true)
    // 3 mine
    lineToGame("3 4 4").isPossible should be(true)
    // 4 mine
    lineToGame("4 4 4").isPossible should be(true)
    // 5 mine
    lineToGame("5 4 4").isPossible should be(true)
    // 6 mine
    lineToGame("6 4 4").isPossible should be(true)
    // 7 mine
    lineToGame("7 4 4").isPossible should be(true) // 9개 이므로 정사각형을 만들 수 있으므로 가능 
    // 8 mine
    lineToGame("8 4 4").isPossible should be(true)
    // 9 mine
    lineToGame("9 4 4").isPossible should be(false)
    // 10 mine
    lineToGame("10 4 4").isPossible should be(true)
    // 11 mine
    lineToGame("11 4 4").isPossible should be(false)
    // 12 mine
    lineToGame("12 4 4").isPossible should be(false)
    // 13 mine
    lineToGame("13 4 4").isPossible should be(false)
    // 14 mine
    lineToGame("14 4 4").isPossible should be(false)
    // 15 mine
    lineToGame("15 4 4").isPossible should be(true)
    // 16 mine
    lineToGame("16 4 4").isPossible should be(false)
  }
}