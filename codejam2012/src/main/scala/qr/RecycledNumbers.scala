package qr

import scala.io.Source

object RecycledNumbers {

  val in = Source.fromFile("res/C-large-practice.in").getLines

  case class Range(line: String) {
    val min = line.split(" ")(0).toInt
    val max = line.split(" ")(1).toInt
    val distinctSet = for (n <- min to max if (n.toString.distinct.length == n.toString.length && isAscending(n.toString))) yield n

    val testSet = for (n <- min to max; m <- n + 1 to max if (n < m) && (n.toString.length == m.toString.length) && (n.toString.sorted == m.toString.sorted) && isTrail(n.toString)) yield n
    println(testSet.toSet)

    def isTrail(str: String) = {
      loop(str.head, str.tail)
    }

    def loop(c: Char, str: String): Boolean = {
      if (str.isEmpty()) true
      else if (c == str.head) false
      else {
        loop(str.head, str.tail)
      }
    }

    def isAscending(n: String): Boolean = {
      n.sorted == n
    }
  }

  def main(args: Array[String]) {
    val n = in.next.toInt
    val ranges = in.take(n).toList
    val a = Range("100 500")

    println(a.isAscending("12345"))
    println(a.isAscending("126345"))

    // ranges.map(Range(_))
    //    ranges.map(Range(_).getRecycledPairCount)
  }

}