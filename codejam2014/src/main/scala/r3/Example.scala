package r3

import java.io.PrintWriter
import scala.io.Source
import r3.Util
import Util.StringListWrapper
import Util.StringWrapper

object Example {
  import Util._

  val inFile = "fileName.in"
  val outFile = "fileName.out"

  val in = Source.fromFile(inFile).getLines

  def printRes(res: Seq[String]) {
    val writer = new PrintWriter("outFile")
    for (i <- 1 to res.size) {
      writer println s"Case #$i: " + res(i - 1)
    }
    writer.flush()
    writer.close()
  }

  def main(args: Array[String]) {
    val res = "1 2 3".toIntList
    println(res)

    val lines = """1 2 3 4
5 6 7 8
9 10 11 12""".lines.toList

    val grid = lines.toIntGrid
    println(grid)
    println(grid.rotateRight)

    val grid2 = lines.toGrid(a => a)

    println(grid2)

    val res2 = List("1", "2", "3")
    printRes(res2)
  }
}