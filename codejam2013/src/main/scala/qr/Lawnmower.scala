package qr

import scala.io.Source
import java.io.PrintWriter
import scala.Array.canBuildFrom

// https://code.google.com/codejam/contest/2270488/dashboard#s=p1
object Lownmower {

  type Lawn = Seq[Seq[Int]]

  val in = Source.fromFile("res/codejam2012/B-large-practice.in").getLines
  val writer = new PrintWriter("res/codejam2012/B-large-practice.out")

  def main(args: Array[String]) {
    val caseCount = in.next.toInt
    val res = testAllCases(caseCount)
    writeToFile(res.mkString("\n"))
  }

  def testAllCases(c: Int) = for (i <- 1 to c) yield formatResult(i, judge(nextLawn))

  def nextLawn: Lawn = {
    val s = in.next()
    val Array(m, n) = s.split(" ").map(_.toInt)
    val lines = in.take(m).toSeq
    // String("2 2 2 3") => "Seq[Int](2, 2, 2, 3)"
    def strLineToIntSeq(str: String) = str.split(" ").map(_.toInt).toSeq
    lines map strLineToIntSeq
  }

  def formatResult(i: Int, b: Boolean) = if (b) { s"""Case #$i: YES""" } else { s"""Case #$i: NO""" }

  def judge(l: Lawn) = {
    // y, x 셀을 중심으로 가로열/세로열에서 현재 셀의 잔디의 높이가 가장 높은지 확인. 둘 모두 false 일 경우 존재 불가능한 배치 
    def checkPossibility(y: Int, x: Int): Boolean = {
      val (row, col) = (l(y), l.map(_(x)))
      val cur = l(y)(x)
      row.forall(cur >= _) || col.forall(cur >= _)
    }

    val results = for {
      y <- Range(0, l.size)
      x <- Range(0, l(0).size)
    } yield checkPossibility(y, x)

    results.forall(_ == true)
  }

  def writeToFile(str: String) {
    writer.print(str)
    writer.flush()
    writer.close()
  }
}