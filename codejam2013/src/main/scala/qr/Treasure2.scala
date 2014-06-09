package qr

import scala.Array.canBuildFrom
import scala.io.Source

object Treasure2 {

  var isFirst = true;

  def isPossible(keys: List[Int], chests: List[Chest]): Boolean = {
    val keySum = keys ++ chests.flatMap(_.keys)
    val keyChests = chests.map(_.kType)
    val notHave = for (k <- keyChests if !keySum.contains(k)) yield k
    if (!notHave.isEmpty) return false

    def loop(keys: List[Int], chests: List[Chest]): Boolean = {
      if (chests == Nil) return true
      val chest = chests.head
      keys.find(_ == chest.kType) match {
        case None => return false
        case Some(key) => {
          val restKey = keys diff List(key)
          loop(restKey, chests.tail)
        }
      }
    }
    loop(keySum, chests)
  }

  type Order = List[Int]
  case class Chest(order: Int, kType: Int, keys: List[Int])

  case class Map(keys: List[Int], chests: List[Chest]) {
    def solve: Option[Order] = open(keys, 0, chests, Nil)

    def open(keys: List[Int], chestIdx: Int, chests: List[Chest], order: List[Int]): Option[List[Int]] = {
      //      println("chest: " + chests)
      //      println("keys: " + keys)
      if (chests == Nil) return Some(order)
      if (keys == Nil) return None
      if (chestIdx == chests.size) return None
      if (!isPossible(keys, chests)) return None

      val optChest = chests.drop(chestIdx).find(chest => keys.contains(chest.kType))
      if (optChest == None) return open(keys, chestIdx + 1, chests, order)
      else {
        val chest = optChest.get
        val key = chest.kType
        val newKeys = (keys diff List[Int](key)) ++ chest.keys
        val newChests = chests diff List[Chest](chest)
        val newOrder = order :+ chest.order
        val res = open(newKeys, 0, newChests, newOrder)
        if (res == None) {
          return open(keys, chestIdx + 1, chests, order)
        } else return res
      }
    }
  }

  def main(args: Array[String]) {
    val maps: Seq[Map] = parseLines(lines)
//    maps.foreach { map =>
//      map.solve match {
//        case None => println("Impossible")
//        case Some(l) => println(l)
//      }
//    }
//    
    maps(8).solve
  }

  implicit class StringWrapper(str: String) {
    def toIntArr: Array[Int] = str.split(" ").map(_.toInt)
  }

  val lines = Source.fromFile("D-small-practice.in").getLines

  def parseLines(lines: Iterator[String]): List[Map] = {
    val n = lines.next.toInt
    val maps = for {
      i <- 1 to n
      val Array(_, chestNum) = lines.next.toIntArr
      val keys = lines.next.toIntArr.toList
      val chests = {
        val ls = lines.take(chestNum).toList
        (1 to chestNum).map(cn => parse(cn)(ls(cn - 1)))
      }
    } yield Map(keys, chests.toList)
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
