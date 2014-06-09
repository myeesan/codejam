package qr

import java.io.PrintWriter
import scala.io.Source

object MinesweeperMaster {

  val lines = Source.fromFile("sampleC.in").getLines
  val writer = new PrintWriter("sampleC.out")

  def lineToGame(line: String): Game = {
    val Array(mine, col, row) = line.split(" ").map(_.toInt)
    Game(mine, col, row)
  }

  case class Game(mine: Int, col: Int, row: Int) {
    def hasOneLine = (col == 1 || row == 1)
    def minLine = Math.min(col, row)
    def space = (col * row) - mine
    def isPossible: Boolean = {
      if (mine >= col * row) false
      else if (mine == 0) true
      else if (space == 1) true
      else if (minLine == 1) col * row > mine
      else if (minLine == 2) space != 2 && space % 2 == 0
      else if (minLine == 3) space != 3 && space % 3 == 0
      else if (minLine >= 4) {
        (space % 8 == 0) || (space % 9 == 0)
      } else false
    }
  }

  def main(args: Array[String]) {
    val caseNum = lines.next.toInt
    val games = lines.take(caseNum).map(lineToGame)
    games.foreach(println)
  }

}