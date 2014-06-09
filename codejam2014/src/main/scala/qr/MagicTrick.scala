package qr

import scala.io.Source
import java.io.PrintWriter

object MagicTrick {

  // 0. 맞는지 확인 (1번 문제의 경우, 속였을 가능성이 있지만 답이 나왔으므로)
  // 1. 치팅 체크
  // 2. 마술사 실수 체크
  val lines = Source.fromFile("A-small-attempt0.in").getLines
  val writer = new PrintWriter("A-small-attempt0.out")

  def writeRess(ress: Seq[String]) {
    ress.foreach(writer.println(_))
    writer.flush()
    writer.close()
  }

  def linesToBoard(lines: List[String]): Board = {
    val selectedNum = lines.head.toInt
    val rows: List[List[Int]] = lines.tail.toList.map(_.split(" ").toList.map(_.toInt))
    Board(rows, selectedNum)
  }

  trait Status {
    def toRes: String
  }
  case class Result(n: Int) extends Status {
    override def toRes = n.toString
  }
  case object Mistake extends Status {
    override def toRes = "Bad magician!"
  }
  case object Cheated extends Status {
    override def toRes = "Volunteer cheated!"
  }

  case class Game(b1: Board, b2: Board) {
    def tryFind: Status = {
      b1.row intersect b2.row match {
        case Nil => Cheated
        case List(n) => Result(n)
        case n :: ns => Mistake
      }
    }
  }

  case class Board(nums: List[List[Int]], rowNum: Int) {
    def row = nums(rowNum - 1)
    override def toString = "\n" + rowNum + "\n" + nums.mkString("\n")
  }

  def parseGames(n: Int): Seq[Game] = for {
    i <- 1 to n
    val board1 = linesToBoard(lines.take(5).toList)
    val board2 = linesToBoard(lines.take(5).toList)
  } yield Game(board1, board2)

  def formatRes(n: Int, resStr: String): String = {
    s"Case #$n: " + resStr
  }

  //Case #1: 7
  //Case #2: Bad magician!
  //Case #3: Volunteer cheated!
  def main(args: Array[String]) {
    val caseNum = lines.next.toInt
    val gameRess = parseGames(caseNum).map(_.tryFind.toRes)

    val ress = for {
      i <- 1 to caseNum
      val res = formatRes(i, gameRess(i - 1))
    } yield res

    writeRess(ress)
  }

}