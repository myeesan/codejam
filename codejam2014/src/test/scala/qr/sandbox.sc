import scala.collection.immutable.ListSet
import scala.collection.immutable.SortedSet
import scala.collection.generic.SortedSetFactory
import scala.collection.mutable.ListBuffer

object sandbox {

  val a = "abbcc" groupBy (a => a)                //> a  : scala.collection.immutable.Map[Char,String] = Map(b -> bb, a -> a, c ->
                                                  //|  cc)

  val b = "aabcc" groupBy (a => a)                //> b  : scala.collection.immutable.Map[Char,String] = Map(b -> b, a -> aa, c ->
                                                  //|  cc)

  val c = a.toList.map(_._2.size)                 //> c  : List[Int] = List(2, 1, 2)
  val d = b.toList.map(_._2.size)                 //> d  : List[Int] = List(1, 2, 2)

  val l = List("abbcc", "aabcc")                  //> l  : List[String] = List(abbcc, aabcc)

  val t = List("dwpppadpbmiyhxkkjveanoixud", "ddddwwpaadpbbbbbbbbbbbbbmmmiiiiiiiyyhhhhhhhhxxkkkkkkkkkkkkjjjjjjjjvvvvveeeaannnnnnoiixuuuudd")
                                                  //> t  : List[String] = List(dwpppadpbmiyhxkkjveanoixud, ddddwwpaadpbbbbbbbbbbbb
                                                  //| bmmmiiiiiiiyyhhhhhhhhxxkkkkkkkkkkkkjjjjjjjjvvvvveeeaannnnnnoiixuuuudd)
                                                  
  def slice(str: String) = {
    val b = ListBuffer[ListBuffer[Char]]()
    for(c <- str){
      if(!b.isEmpty && b.last.last == c){
        b.last += c
      } else b += ListBuffer(c)
    }
    b.toList.map(_.toList)
  }                                               //> slice: (str: String)List[List[Char]]
  
  slice(t(1))                                     //> res0: List[List[Char]] = List(List(d, d, d, d), List(w, w), List(p), List(a,
                                                  //|  a), List(d), List(p), List(b, b, b, b, b, b, b, b, b, b, b, b, b), List(m, 
                                                  //| m, m), List(i, i, i, i, i, i, i), List(y, y), List(h, h, h, h, h, h, h, h), 
                                                  //| List(x, x), List(k, k, k, k, k, k, k, k, k, k, k, k), List(j, j, j, j, j, j,
                                                  //|  j, j), List(v, v, v, v, v), List(e, e, e), List(a, a), List(n, n, n, n, n, 
                                                  //| n), List(o), List(i, i), List(x), List(u, u, u, u), List(d, d))
  
  def mapToCharNum(str: String): List[Int] = {
    (slice(str)).toList.map(_.size)
  }                                               //> mapToCharNum: (str: String)List[Int]

  
  def toVert(ls: List[List[Int]]): List[List[Int]] = {
    val size = ls(0).size
    val result = for {
      i <- 0 until size
    } yield ls.map(_(i))
    result.toList
  }                                               //> toVert: (ls: List[List[Int]])List[List[Int]]

  l.map(mapToCharNum(_))                          //> res1: List[List[Int]] = List(List(1, 2, 2), List(2, 1, 2))

  val vert = toVert(t.map(mapToCharNum(_)))       //> vert  : List[List[Int]] = List(List(1, 4), List(1, 2), List(3, 1), List(1, 
                                                  //| 2), List(1, 1), List(1, 1), List(1, 13), List(1, 3), List(1, 7), List(1, 2)
                                                  //| , List(1, 8), List(1, 2), List(2, 12), List(1, 8), List(1, 5), List(1, 3), 
                                                  //| List(1, 2), List(1, 6), List(1, 1), List(1, 2), List(1, 1), List(1, 4), Lis
                                                  //| t(1, 2))
  
  2 + 1 + 5 + 7 + 1 + 3 + 2 + 2 + 7 + 4 + 12 + 2 + 7 + 1 + 10 + 0 + 4
                                                  //> res2: Int = 70
  def findMinMove(nums: List[Int]): Int = {
    val res = for {
      i <- nums.min to nums.max
    } yield nums.map(n => Math.abs(n - i))
    res.map(_.sum).min
  }                                               //> findMinMove: (nums: List[Int])Int
  
  findMinMove(List(1, 4, 6))                      //> res3: Int = 5

  vert.map(findMinMove(_)).sum                    //> res4: Int = 70
  findMinMove(List(1, 2, 1))                      //> res5: Int = 1
  findMinMove(List(2, 3))                         //> res6: Int = 1
  findMinMove(List(2, 2, 2))                      //> res7: Int = 0

  ""                                              //> res8: String("") = ""

  "aabbcc".toList.foldLeft(List[Char]()) { (a, b) => if (a.lastOption == b) a else a :+ b }
                                                  //> res9: List[Char] = List(a, a, b, b, c, c)
  val set1 = scala.collection.mutable.LinkedHashSet.empty ++ "aabbccka"
                                                  //> set1  : scala.collection.mutable.LinkedHashSet[Char] = Set(a, b, c, k)
  val set2 = scala.collection.mutable.LinkedHashSet.empty ++ "aabbcca"
                                                  //> set2  : scala.collection.mutable.LinkedHashSet[Char] = Set(a, b, c)
  set1.toList == set2.toList                      //> res10: Boolean = false

  
}