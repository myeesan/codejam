package qr

import scala.io.Source
import java.io.PrintWriter
import scala.annotation.tailrec

object CookieClickerAlpha {

  val lines = Source.fromFile("B-small-attempt0.in").getLines
  val writer = new PrintWriter("B-small2.out")

  def writeRess(ress: Seq[String]) {
    ress.foreach(writer.println(_))
    writer.flush()
    writer.close()
  }

  def formatRes(n: Int, resStr: String): String = {
    s"Case #$n: " + resStr
  }

  case class Farm(cost: Double, f: Double)

  case class Game(farm: Farm, x: Double) {

    def calcTime(lim: Int): Double = {
      var consumed: Double = 0d
      var points: Double = 2d

      loop(lim, 0d, 2d)

    }

    def loop(c: Int, consumed: Double, points: Double): Double = {
      if (c != 0) {
        val c2 = consumed + farm.cost / points
        val p2 = points + farm.f
        loop(c - 1, c2, p2)
      }
      val res = consumed + x / points
      res
    }

    def findMinTime(count: Int): Double = {
      val current = calcTime(count)
      val next = calcTime(count + 1)
      if (current <= next) current
      else findMinTime(count + 1)
    }
  }

  def lineToGame(line: String): Game = {
    val Array(c, f, x) = line.split(" ").map(_.toDouble)
    Game(Farm(c, f), x)
  }

  def main(args: Array[String]) {
    val caseNum = lines.next.toInt
    val games = lines.take(caseNum).toList.map(lineToGame)
    val gameRess = games.map(_.findMinTime(0).toString)

    val ress = for {
      i <- 1 to caseNum
      val res = formatRes(i, gameRess(i - 1))
    } yield res

    writeRess(ress)
  }
}