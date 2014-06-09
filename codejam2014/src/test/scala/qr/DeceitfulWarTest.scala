package qr

import org.scalatest.FunSuite
import org.scalatest.Matchers

import qr.DeceitfulWar._
import scala.util.Random

class DeceitfulWarTest extends FunSuite with Matchers {

  test("Game opt") {
    val game1 = toGame(List("1", "0.5", "0.6"))
    val game2 = toGame(List("2", "0.7 0.2", "0.8 0.3"))
    val game3 = toGame(List("3", "0.5 0.1 0.9", "0.6 0.4 0.3"))
    val game4 = toGame(List("9", "0.186 0.389 0.907 0.832 0.959 0.557 0.300 0.992 0.899", "0.916 0.728 0.271 0.520 0.700 0.521 0.215 0.341 0.458"))

    game1.playOpt should be(0)
    game2.playOpt should be(0)
    game3.playOpt should be(1)
    game4.playOpt should be(4)
  }

  test("Game dec") {
    println("========================")
    val game1 = toGame(List("1", "0.5", "0.6"))
    val game2 = toGame(List("2", "0.7 0.2", "0.8 0.3"))
    val game3 = toGame(List("3", "0.5 0.1 0.9", "0.6 0.4 0.3"))
    val game4 = toGame(List("9", "0.186 0.389 0.907 0.832 0.959 0.557 0.300 0.992 0.899", "0.916 0.728 0.271 0.520 0.700 0.521 0.215 0.341 0.458"))

    game1.playDec should be(0)
    game2.playDec should be(1)
    game3.playDec should be(2)
    game4.playDec should be(8)
    println("========================")
  }

  test("Game dec2") {
    val game1 = toGame(List("3", "0.11 0.12 0.13", "0.22 0.23 0.24"))
    val game2 = toGame(List("3", "0.22 0.23 0.24", "0.11 0.12 0.13"))
    game1.playDec should be(0)
    game2.playDec should be(3)
  }

  test("Game dec3") {
    val game1 = toGame(List("9", "0.15 0.25 0.35 0.45 0.55 0.65 0.75 0.85 0.95", "0.10 0.20 0.30 0.40 0.50 0.60 0.70 0.80 0.90"))
    game1.playDec should be(9)
    game1.playOpt should be(1)
  }

  test("Large") {
    val rand = Random
    val lList1 = List.fill(1000)(rand.nextFloat)
    val lList2 = List.fill(1000)(rand.nextFloat)
    val game = toGame(List("1000", lList1.mkString(" "), lList2.mkString(" ")))
    println(game.playOpt + " " + game.playDec)
  }

  test("KenBlock") {
    val blocks = KenBlocks(List(1f, 2f, 3f, 4f))

    blocks.removeLargest should be(KenBlocks(List(1f, 2f, 3f).reverse))
  }
}