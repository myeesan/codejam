package r3

import java.io.PrintWriter
import scala.io.Source
import scala.Array.canBuildFrom

object Enclosure {

  val inFile = "C-small-attempt1.in"
  val outFile = "C-small-attempt1.out"
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
    def toIntList: List[Int] = str.split(" ").toList.map(_.toInt)
    def toTest = {
      val Array(n, m, k) = str.split(" ").map(_.toInt)
      Test(n, m, k)
    }
  }

  // k: 전체 intersection의 수
  case class Test(n: Int, m: Int, k: Int) {
    val min = Math.min(n, m)
    val max = Math.max(n, m)

    def solve: Int = {
      if(min == 1) 2
      else if(min == 2) 4
      else if(min % 2 == 0) {
        val k1 = k - 2
        val k2 = k1 / 3
        val res = k1 + k2 * 2
        if(k % 2 == 1) {
          res - 1
        } else res
      }
      else {
    	 k / 2
      }
    }
    
    
  }

  def main(args: Array[String]) {
    val n = in.next.toInt
    val res = in.take(n).toList.map(_.toTest)
    printRes(res.map(_.toString))
  }

}