package qr

import org.scalatest.Matchers
import org.scalatest.FunSuite

import qr.MinesweeperMaster._

// 맵 생성 조
// 2x2 이상에서 5개 있는 경우는 1이 존재하는 Cell을 C표시한다

// XXX    XXX
// X.. => X32
// .C.    310

class MinesweeperMasterTest extends FunSuite with Matchers {

  // 마인이 0이면 무조건 통과
  test("if mine is 0") {
    val g1 = Game(0, 1, 1)
    val g2 = Game(0, 1, 2)
    val g3 = Game(0, 2, 2)
    val g4 = Game(0, 10, 10)

    g1.isPossible should be(true)
    g2.isPossible should be(true)
    g3.isPossible should be(true)
    g4.isPossible should be(true)
  }

  // 마인이 한개이면 무조건 통과
  test("if min is 1") {
    val g1 = Game(1, 1, 1)
    val g2 = Game(1, 1, 2)

    g1.isPossible should be(false)
    g2.isPossible should be(true)
  }

  // 가로나 세로 둘 중의 하나가 1 인 경우는 지뢰 개수가 전체 칸보다 1이상 적으면 가능
  test("is Possible ethier row or col has 1 line") {
    val game1 = Game(5, 1, 5) // 지뢰가 전체 칸보다 많으면 불가능 
    val game2 = Game(4, 1, 5) // 지뢰가 전체 칸보다 적으면 가능
    val game3 = Game(3, 1, 5)

    game1.isPossible should be(false)
    game2.isPossible should be(true)
    game3.isPossible should be(true)
  }

  test("1x1") {
    val game1 = Game(1, 1, 1)
    val game2 = Game(0, 1, 1)

    game1.isPossible should be(false)
    game2.isPossible should be(true)
  }

  // X..C    X200
  // X... => X300
  // XX..    XX32
  // XXXX    XXXX
  //

  // 3개 초과시 빈 칸이 9의 배수라면 가능

  // 가로 세로 최소라인이 2인 경우는 2개를 제외한 4개이상의 여유가 있어야 가능함  
  test("both row and col have more 2 lines") {
    val game1 = Game(0, 2, 3) // s = 6
    val game2 = Game(1, 2, 3) // s = 5 // 마인이 1개이므로 true
    val game3 = Game(2, 2, 4) // s = 6
    val game4 = Game(6, 2, 3) // s = 3
    val game5 = Game(6, 2, 4) // s = 2 따라서 불가
    val game6 = Game(2, 2, 2) // s = 2 false

    game1.isPossible should be(true)
    game2.isPossible should be(false)
    game3.isPossible should be(true)
    game4.isPossible should be(false)
    game5.isPossible should be(false)
    game6.isPossible should be(false)
  }

  // XXX..C    XXX100
  // XXX... => XXX100
  test("작은 라인이 2개이고 빈 칸이 4개 이상인 경우 가능함") {
    val game0 = Game(7, 2, 4)
    val game1 = Game(4, 2, 4)
    val game2 = Game(3, 2, 4)
    val game3 = Game(2, 2, 4)
    
    game0.isPossible should be(true)
    game1.isPossible should be(true)
    game2.isPossible should be(false)
    game3.isPossible should be(true)
  }

  // 가로 세로 최소라인이 3인 경우는 3개 를 제외한 3의 배수만큼의 여유가 있어야 가능함
  test("min 3 has should have more 6 space") {
    val game1 = Game(3, 3, 3)
    val game2 = Game(4, 3, 3)
    val game3 = Game(5, 3, 4)
    val game4 = Game(6, 3, 5)
    val game5 = Game(6, 3, 3)
    val game6 = Game(8, 3, 3)

    game1.isPossible should be(true)
    game2.isPossible should be(false)
    game3.isPossible should be(false)
    game4.isPossible should be(true)
    game5.isPossible should be(false)
    game6.isPossible should be(true)
  }
  
  

}