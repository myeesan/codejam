package qr

import java.io.PrintWriter
import scala.io.Source

object DeceitfulWar {

  val lines = Source.fromFile("D-large.in").getLines
  val writer = new PrintWriter("D-large.out")

  case class NaomiBlocks(blocks: List[Float]) {
    def isEmpty = blocks.isEmpty

    def popSmall: (Float, NaomiBlocks) = {
      val small = blocks.min
      val rest = blocks diff List(small)
      (small, NaomiBlocks(rest))
    }

    def size: Int = blocks.size

    def rest(n: Float): NaomiBlocks = {
      val smallWinBlock = blocks.filter(_ > n).min
      val rest = blocks diff List(smallWinBlock)
      NaomiBlocks(rest)
    }

    def hasNoLargerThan(kb: KenBlocks): Boolean = {
      if (blocks == Nil) true
      else blocks.max < kb.min
    }

    def hasLargerThan(weight: Float): Boolean = {
      blocks.find(_ > weight) match {
        case None => false
        case Some(_) => true
      }
    }
  }

  
  case class KenBlocks(blocks: List[Float]) {

    def canWinnable(n: Float): Boolean = {
      blocks.filter(_ > n) != Nil
    }

    def rest(n: Float): KenBlocks = {
      val smallWinBlock = blocks.filter(_ > n).min
      val rest = blocks diff List(smallWinBlock)
      KenBlocks(rest)
    }

    def removeLargest = KenBlocks(blocks.sorted.reverse.tail) // 가장 큰 수만 제거

    def min = blocks.min

    def popSmall: (Float, KenBlocks) = {
      val sorted = blocks.sorted
      val min = sorted.head
      val tail = sorted.tail
      (min, KenBlocks(tail))
    }

    def size = blocks.size
  }

  case class Game(blockNum: Int, naomiBlocks: NaomiBlocks, kenBlocks: KenBlocks) {

    def playOpt: Int = playOpt(naomiBlocks, kenBlocks)
    private def playOpt(nb: NaomiBlocks, kb: KenBlocks): Int = {
      if (nb.isEmpty) 0
      else {
        val (small, restNb) = nb.popSmall
        if (kb.canWinnable(small)) {
          playOpt(restNb, kb.rest(small))
        } else {
          nb.size
        }
      }
    }

    def playDec: Int = playDec(naomiBlocks, kenBlocks)
    // 나오미는 가장 작은 것을 내고, 켄의 가장 큰 수를 유도한다.
    private def playDec(nb: NaomiBlocks, kb: KenBlocks): Int = {
      if (kb.size == 0) {
        blockNum
      } else {
        val (kbSmall, kbRest) = kb.popSmall
        if (!nb.hasLargerThan(kbSmall)) {
          blockNum - nb.size
        } else {
          val rest = nb.rest(kbSmall)
          playDec(rest, kbRest)
        }
      }
    }
  }

  def toGame(lines: List[String]): Game = {
    val blockNum = lines.head.toInt
    val naomi = NaomiBlocks(lines(1).split(" ").toList.map(_.toFloat).sorted)
    val ken = KenBlocks(lines(2).split(" ").toList.map(_.toFloat).sorted)
    Game(blockNum, naomi, ken)
  }

  def writeRess(ress: Seq[String]) {
    ress.foreach(writer.println(_))
    writer.flush()
    writer.close()
  }

  def formatRes(n: Int, resStr: String): String = {
    s"Case #$n: " + resStr
  }

  def main(args: Array[String]) {
    val caseNum = lines.next.toString.toInt

    val games = for {
      i <- 1 to caseNum
      val gameStr = lines.take(3).toList
      val game = toGame(gameStr)
    } yield formatRes(i, game.playDec + " " + game.playOpt)

    writeRess(games)
  }
}