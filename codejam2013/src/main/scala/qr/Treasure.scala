package qr

import scala.io.Source
import scala.Array.canBuildFrom

object Treasure {

  type Order = List[Int]
  type KeyType = Int

  case class KeyHolder(keys: List[KeyType]) extends Iterable[KeyType] {
    def iterator = keys.iterator

    def -(key: KeyType) = KeyHolder(keys diff List(key))

    def findMatchType(chests: List[Chest]): Option[Chest] = {
      loop(keys, chests)
    }

    override def drop(d: Int) = KeyHolder(keys.drop(d))

    private def loop(keys: List[KeyType], chests: List[Chest]): Option[Chest] =
      chests match {
        case Nil => None
        case head :: tail if keys contains (head.t) => Some(head)
        case _ :: tail => loop(keys, tail)
      }
  }

  case class Chest(order: Int, t: KeyType, contains: List[KeyType])

  case class Map(kh: KeyHolder, cs: Seq[Chest]) {

    type Tup = Option[(KeyHolder, List[Chest], List[Int])]

    def solve: Option[Order] = solve(kh, 0, cs.toList, Nil)

    def solve(keyHolder: KeyHolder, d: Int, chests: List[Chest], order: Order): Option[Order] = {
      if (chests == Nil) {
        println("Chest is Nil")
        Some(order)
      } else if (keyHolder.keys == Nil) {
        // TODO: chest가 남은 경우 다음 시도
        None
      } else {
        keyHolder.drop(d) findMatchType chests match {
          case Some(chest) =>
            val key = chest.t
            val subChests = chests diff List(chest)
            val subKeys = KeyHolder((keyHolder - key).keys ++ chest.contains)
            val chestOrder = chest.order
            println("Some match key")
            println("order: " + order)
            println("chest: " + chests)
            println("keys: " + keyHolder)
            solve(subKeys, 0, subChests, order :+ chestOrder) // TODO: reverse로 나중에 뒤집
          case None => {
            println("No MatchKey")
            None
          }
        }
      }
    }
  }

  def main(args: Array[String]) {
    val maps: Seq[Map] = parseLines(lines)

    //    println(maps(0).solve)
    maps foreach println
  }

  implicit class StringWrapper(str: String) {
    def toIntArr: Array[Int] = str.split(" ").map(_.toInt)
  }

  val lines = Source.fromFile("res/qr/D-small-practice.in").getLines

  def parseLines(lines: Iterator[String]): List[Map] = {
    val n = lines.next.toInt
    val maps = for {
      i <- 1 to n
      val Array(_, chestNum) = lines.next.toIntArr
      val keyHolder = KeyHolder(lines.next.toIntArr.toList)
      val chests = {
        val ls = lines.take(chestNum).toList
        (1 to chestNum).map(cn => parse(cn)(ls(cn - 1)))
      }
    } yield Map(keyHolder, chests)
    maps.toList
  }

  def parse(n: Int)(str: String): Chest = {
    val line = str.split(" ").map(_.toInt).toList
    line match {
      case head :: tail => Chest(n, head, tail.tail)
      case _ => throw new Exception
    }
  }
}