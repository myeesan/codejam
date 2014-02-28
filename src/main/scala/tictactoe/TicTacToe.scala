package tictactoe

// https://code.google.com/codejam/contest/2270488/dashboard#s=p0
object TicTacToe {

  val in = scala.io.Source.fromFile("A-large-practice.in").getLines
  val writer = new java.io.PrintWriter("A-large-practice.out")

  def nextBoard(lines: Iterator[String]) = lines.take(5) filterNot (_ == "")

  def combis(board: Iterator[String]) = {
    val rows = board.toSeq
    val cols = for (i <- 0 to 3) yield { for (j <- 0 to 3) yield rows(j)(i) }.mkString
    val \ = (0 to 3).map(i => rows(i)(i)).mkString
    val / = (0 to 3).map(i => rows(i)(3 - i)).mkString
    rows ++ cols ++ Seq(\, /)
  }

  def judge(l: Seq[String])(cond: Set[Char] => String): String = {
    val sets = l.map(_.toSet - 'T').toSet.filter(_.size == 1)
    cond(sets.flatten)
  }

  def main(args: Array[String]) {
    val n = in.next().toInt

    for (i <- 1 to n) {
      val cs = combis(nextBoard(in))
      val hasDot = cs.toList.flatten.contains('.') // TODO: refactor
      val res = judge(cs) {
        set =>
          if (set == Set() && hasDot) "Game has not completed"
          else if (set == Set() && !hasDot) "Draw"
          else if (set == Set('X')) "X won"
          else if (set == Set('O')) "O won"
          else "Game has not completed"
      }
      writer.println(s"""Case #$i: """ + res)
    }
    writer.flush()
    writer.close()
  }
}