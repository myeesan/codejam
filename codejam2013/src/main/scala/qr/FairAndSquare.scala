package qr

import java.io.PrintWriter
import scala.annotation.tailrec
import scala.io.Source

object FairAndSquare {

  case class BigRangeCounter(val min: BigInt, val max: BigInt) {
    var count = 0
    def countIfInRange(n: BigInt) = if (min <= n && max >= n) count += 1
  }

  val in: Iterator[String] = Source.fromFile("res/qr/C-large-practice-2.in").getLines

  val writer = new PrintWriter("res/qr/C-large-practice-2.out")

  def main(args: Array[String]) {

    genAllFairAndSqureNums(maxDigit / 2) foreach { num =>
      ranges.foreach { counter =>
        counter countIfInRange num
      }
    }

    write { (counter, index) =>
      s"""Case #${index + 1}: ${counter.count}"""
    }
  }

  def write(f: (BigRangeCounter, Int) => String) = {
    ranges.zipWithIndex.map(f.tupled).foreach(writer.println)
    writer.flush()
  }

  val caseNum: Int = in.next.toInt

  val ranges: Seq[BigRangeCounter] = {
    in.take(caseNum).map { line =>
      val Array(min, max) = line.split(" ")
      BigRangeCounter(BigInt(min), BigInt(max))
    }.toSeq
  }

  val maxDigit = ranges.reduce((a, b) => if (a.max > b.max) a else b).max.toString.length

  val e1Builder = (str: String) => buildPalin(str)
  val e2Builder = (str: String) => buildPalin(str, "2")
  val e1c0Builder = (str: String) => buildPalin(str, center = "0")
  val e1c1Builder = (str: String) => buildPalin(str, center = "1")
  val e1c2Builder = (str: String) => buildPalin(str, center = "2")
  val e2c0Builder = (str: String) => buildPalin(str, "2", "0")
  val e2c1Builder = (str: String) => buildPalin(str, "2", "1")

  def genOddPalinRoots(d: Int): Seq[BigInt] = {
    val pad = (d - 3) / 2
    genPalins(e1c0Builder)(pad) ++
      genPalins(e1c1Builder)(pad) ++
      genPalins(e1c2Builder)(pad, 1) ++
      genPalins(e2c0Builder)(pad, 0) ++
      genPalins(e2c1Builder)(pad, 0)
  }

  def genEvenPalinRoots(d: Int): Seq[BigInt] = {
    val pad = (d - 2) / 2
    genPalins(e1Builder)(pad) ++ genPalins(e2Builder)(pad, 0)
  }

  /**
   * @param halfOfMaxDigit 자리수를 2로 나눈 값
   * @return Seqeunce of "fair and square number"
   */
  def genAllFairAndSqureNums(halfOfMaxDigit: Int): Seq[BigInt] = {
    implicit def stringToBigInt(str: String): BigInt = BigInt(str)
    implicit class IntWrapper(val i: Int) { def isEven = i % 2 == 0 }

    @tailrec
    def loop(n: Int, xs: List[BigInt]): Seq[BigInt] = {
      if (n > halfOfMaxDigit) xs
      else if (n == 1) loop(n + 1, xs ++ Seq[BigInt]("1", "2", "3"))
      else if (n isEven) loop(n + 1, xs ++ genEvenPalinRoots(n))
      else loop(n + 1, xs ++ genOddPalinRoots(n))
    }
    loop(1, Nil).map(bi => bi * bi)
  }

  /**
   * @param space 가능한 여백
   * @param nLim  1의 최대 갯수
   */
  def combisSeq(space: Int, nLim: Int): Seq[Seq[Int]] =
    (0 to nLim).flatMap(n => (0 until space).combinations(n).toSeq)

  /**
   * @param str 매핑된 padding
   * @param last 마지막에 붙는 숫자 1 또는 2
   * @param center 자리수가 홀수인 경우 가운데 들어오는 숫자 1 또는 2
   */
  def buildPalin(str: String, last: String = "1", center: String = "") =
    (last + str.reverse) + center + (str + last)

  /**
   * @param builder 특성을 구분짓는 함수
   * @param padding 0 또는 1 이 올 수 있는 여백, 짝수의 경우 (자릿수 - 2 / 2) - 1, 홀수의 경우 (자릿수 - 3 / 2) - 1
   * @param nOfOne  1의 최대 개수, builder에 의해 결정
   */
  def genPalins(builder: String => String)(padding: Int, nOfOne: Int = 3): Seq[BigInt] = {
    val indicesSeq = combisSeq(padding, nOfOne)
    indicesSeq.map {
      indices: Seq[Int] =>
        val buf = List.fill(padding)('0').toBuffer
        indices.map(buf(_) = '1')
        BigInt(builder(buf.mkString))
    }
  }
}