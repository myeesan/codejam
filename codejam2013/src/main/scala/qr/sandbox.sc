package treasure

import scala.collection.mutable.ListBuffer

import treasure.Treasure._

object sandbox {

  case class Keys(keys: List[Int]) extends Iterable[Int] {
    def iterator = keys.iterator
  }

  Keys(List(1, 2, 3)) foreach println

  val keys = KeyHolder(List(1, 2, 3))
  List(1, 2, 3, 4, 5)

  KeyHolder(List(1, 2, 3)) == KeyHolder(1 :: List(2, 3))
  List(1, 2, 3) diff List(1, 3)

  List(1, 2, 3) diff List(2)

  def testReturn(i: Int): Option[String] = {
    if (i == 2) return Some("Returned")
    None
  }

  val res = testReturn(2)
  val res2 = testReturn(3)
}