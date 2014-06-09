package qr

import java.io.PrintWriter

import scala.collection.mutable.ListBuffer
import scala.io.Source

object TheRepeater {
  val lines = Source.fromFile("a-large.in").getLines
  val writer = new PrintWriter("a-large.out")

  def writeRess(ress: Seq[String]) {
    ress.foreach(writer.println(_))
    writer.flush()
    writer.close()
  }

  def formatRes(n: Int, resStr: String): String = s"Case #$n: " + resStr

  case class Game(strs: List[String]) {

    def solve: Option[Int] = {
      if (strs.map(_.toSet).toSet.size == 1 && strs.map(merge(_)).toSet.size == 1) {
        val res = toVert(strs.map(mapToCharNum)).map(findMinMove(_)).sum
        Some(res)
      } else {
        None
      }
    }

    def merge(s: String) = {
      val res = s.toList.map(_.toString).reduceLeft((a, b) => if (a.last.toString == b) a else a ++ b)
      println(res)
      res
    }
    def slice(str: String) = {
      val b = ListBuffer[ListBuffer[Char]]()
      for (c <- str) {
        if (!b.isEmpty && b.last.last == c) {
          b.last += c
        } else b += ListBuffer(c)
      }
      b.toList.map(_.toList)
    }

    def mapToCharNum(str: String): List[Int] = {
      (slice(str)).toList.map(_.size)
    }

    def toVert(ls: List[List[Int]]): List[List[Int]] = {
      val size = ls(0).size
      val result = for {
        i <- 0 until size
      } yield ls.map(_(i))
      result.toList
    }

    def findMinMove(nums: List[Int]): Int = {
      val res = for {
        i <- nums.min to nums.max
      } yield nums.map(n => Math.abs(n - i))
      res.map(_.sum).min
    }
  }

  def main(args: Array[String]) {
    val n = lines.next.toInt

    val games = for {
      i <- 1 to n
      val strNum = lines.next.toInt
      val strs = lines.take(strNum).toList
    } yield Game(strs)

    val res = games.map {
      _.solve match {
        case Some(n) => "" + n
        case None => "Fegla Won"
      }
    }

    val resStr = for (i <- 1 to res.size) yield formatRes(i, res(i - 1))
    writeRess(resStr)
  }
}