package qr

import scala.io.Source

object DancingWithGooglers {

  //val in = Source.fromFile(".small.in").getclass()
  val in = Source.fromFile("res/C-large-practice.in")
  val lines = in.getLines

  def happen(a: Int, b: Int, c: Int) = {
    val abc = List(a, b, c)

    abc.max - abc.min <= 2
  }

  def isSurprising(abc: List[Int]) = {
    abc.max - abc.min == 2
  }

  def comb(lim: Int) = {
    val combs = for {
      a <- (1 to lim)
      b <- (1 to lim)
      c <- (1 to lim)
      if (a + b + c == lim)
      if (happen(a, b, c))
    } yield List(a, b, c).sorted
    combs.toSet
  }

  def getSurpNum(l: List[List[Int]]) = {
    l.map(isSurprising(_)).filter(_ == true).size
  }

  def main(args: Array[String]) {
    val n = lines.next.toInt
    for (i <- 1 to n) {
      val line = lines.next.split(" ").toList
      val nOfDancer = line(0).toInt
      val nOfSurprise = line(1).toInt
      val p = line(2).toInt
      val scores = line.drop(3).map(_.toInt)

      val res = for {
        score <- scores
      } yield comb(score).toList
      //      val perm = res.permutations
      //      println(perm.size)
      //      val r2 = perm.filter(getSurpNum(_) == nOfSurprise)
    }
    val s = List(List(List(1, 2, 3), List(4, 5, 6)), List(List((0, 0, 0)))).permutations.toList
    println(s)
  }
}