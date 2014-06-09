package r1

import java.io.PrintWriter
import scala.io.Source

object ChargingChaos {

  val in = Source.fromFile("res/r1/A-large-practice.in").getLines
  val writer = new PrintWriter("res/r1/A-large-practice.out")

  type Flip = List[Boolean]

  case class TestCase(devices: List[String], sockets: List[String]) {
    def solve: String = {
      findNext(findFlips(devices.head), devices.tail) match {
        case None => "NOT POSSIBLE"
        case Some(res) => "" + res.map(_.count(_ == false)).sorted.head
      }
    }

    def findNext(flips: List[Flip], devs: List[String]): Option[List[Flip]] = {
      if (flips == Nil) None
      else if (devs == Nil) Some(flips)
      else {
        val res = findFlips(devs.head)
        val intersections = flips intersect res
        findNext(intersections, devs.tail)
      }
    }

    def findFlips(dev: String): List[Flip] = {
      sockets.map(flipIdx(dev, _)).toList
    }

    def flipIdx(dev: String, soc: String): List[Boolean] = (dev zip soc).map { case (a, b) => a == b }.toList
  }

  def main(args: Array[String]) {
    val n = in.next.toInt;

    val res = for {
      i <- 1 to n
    } yield {
      in.next // skip
      val List(dev, soc) = in.take(2).map(_.split(" ").toList).toList
      TestCase(dev, soc).solve
    }

    (1 to res.size).foreach(i => writer.println(s"Case #$i: ${res(i - 1)}"))
    writer.flush()
  }
}