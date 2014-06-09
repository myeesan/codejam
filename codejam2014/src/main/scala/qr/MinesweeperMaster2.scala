package qr

import java.io.PrintWriter
import scala.io.Source

object MinesweeperMaster2 {
  val lines = Source.fromFile("sampleC.in").getLines
  val writer = new PrintWriter("sampleC.out")

  trait Result {
    def toRes: List[String]
  }

  case class Impossible extends Result {
    override def toRes = List("Impossible")
  }

  case class MineMap(mine: Int, col: Int, row: Int) extends Result {
    def map: List[List[String]] = {
	  val width = Math.max(col, row)
	  val height = Math.min(col, row)
	  var rest = mine
	  ???
    }

    override def toRes = {
      ???
    }
  }

  def rotate[T](list: List[List[T]]) = {
    val width = list.size
    val height = list(0).size
    val flatten = list.flatten
    List.tabulate(height, width) { (w, h) =>
      val idx = h * height + w
      flatten(idx)
    }.reverse
  }

  case class Game(mine: Int, col: Int, row: Int) {
    def minWidth = Math.min(col, row)
    def maxWidth = Math.max(col, row)
    def cellCount = col * row
    def emptyCount = cellCount - mine

    def logic: Boolean = {
      if (emptyCount >= 6 && emptyCount != 7) minWidth >= 2
      else emptyCount >= (minWidth * 2) && !(emptyCount % minWidth == 1)
    }

    def isPossible = {
      if (mine == 0 || emptyCount == 1) true
      else if (minWidth == 1 && emptyCount >= 1) true
      else logic
    }

    def getMap: Option[MineMap] = {
      if (isPossible) Some(MineMap(mine, col, row))
      else None
    }
  }

  def main(args: Array[String]) {
    val caseNum = lines.next.toInt
    val games = lines.take(caseNum).map(lineToGame)
    games.foreach(println)
  }

  def lineToGame(line: String): Game = {
    val Array(mine, col, row) = line.split(" ").map(_.toInt)
    Game(mine, col, row)
  }
}