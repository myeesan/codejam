package r3

import scala.io.Source
import java.io.PrintWriter
import scala.Array.canBuildFrom

object PartElf {
  val inFile = "A-large.in"
  val outFile = "A-large.out"
  val in = Source.fromFile(inFile).getLines

  def printRes(res: Seq[String]) {
    val writer = new PrintWriter(outFile)
    for (i <- 1 to res.size) {
      writer println s"Case #$i: " + res(i - 1)
    }
    writer.flush()
    writer.close()
  }

  implicit class StringWrapper(str: String) {
    def toIntList: List[Long] = str.split(" ").toList.map(_.toLong)
    def toRational: Rational = {
      val Array(n, d) = str.split("/").map(_.toLong)
      Rational(n, d)
    }
  }

  //
  //
  //

  case object Table {
    val table = List.tabulate(40)(a => (1d / (Math.pow(2, a)).toDouble))
    def getAnc(num: Double): Long = {
      table.indexWhere(_ <= num)
    }
  }

  case class Rational(n: Long, d: Long) {
    val num = n.toDouble / d.toDouble
    def solve: String = {
      if (d != 1 && !isValid(d)) {
        "impossible"
      } else {
        "" + Table.getAnc(num)
      }
    }
  }

  def isValid(n: Long): Boolean = {
    if (n == 1) true
    else if (n % 2 != 0) false
    else isValid(n / 2)
  }

  def main(args: Array[String]) {
    val n = in.next.toInt
    val res = in.take(n).toList.map(_.toRational.solve)
    println(isValid(325880643584l))
    printRes(res)
  }
}
