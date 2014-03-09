package codejam.fairandscala

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.matchers.Matcher

class FairAndSquareTest extends FunSuite with Matchers {
  import fairandsquare.FairAndSquare._

  test("combisSeq") {
    combisSeq(1, 0) should be(Seq(Nil))

    combisSeq(1, 1) should be(Seq(Nil, Seq(0)))
    combisSeq(2, 1) should be(Seq(Nil, Seq(0), Seq(1)))

    combisSeq(3, 1) should be(Seq(Nil, Seq(0), Seq(1), Seq(2)))
    combisSeq(3, 2) should be(Seq(Nil, Seq(0), Seq(1), Seq(2), Seq(0, 1), Seq(0, 2), Seq(1, 2)))
  }

  test("buildPalin") {
    buildPalin("1234", "a") should be("a43211234a")
    buildPalin("----", center = "a") should be("1----a----1")
    buildPalin("----") should be("1--------1")
  }

  test("builderTest") {

    e1Builder("00") should be("100001")
    e2Builder("00") should be("200002")
    e1c0Builder("00") should be("1000001")
    e1c1Builder("00") should be("1001001")
    e1c2Builder("00") should be("1002001")
    e2c0Builder("00") should be("2000002")
    e2c1Builder("00") should be("2001002")
  }

  test("genPalins") {
    implicit def stringToBigInt(str: String): BigInt = BigInt(str)
    val padding = 1
    val nOfOne = 1
    genPalins(e1Builder)(padding, nOfOne) should be(Seq[BigInt]("1001", "1111"))
    genPalins(e1Builder)(2, 1).toSet should be(Set[BigInt]("100001", "110011", "101101"))
    genPalins(e1Builder)(2, 2).toSet should be(Set[BigInt]("100001", "110011", "101101", "111111"))
  }

  test("genOddBigInt") {
    val target3 = genOddPalinRoots(3)
    target3 should contain(BigInt("101"))
    target3 should contain(BigInt("111"))
    target3 should contain(BigInt("121"))
    target3 should contain(BigInt("202"))
    target3 should contain(BigInt("212"))
    target3.size should be(target3.toSet.size) // 중복체크

    val target5 = genOddPalinRoots(5)
    target5 should contain(BigInt("10101"))
    target5 should contain(BigInt("11111"))
    target5 should contain(BigInt("20102"))
    target5.size should be(target5.toSet.size)

    val target11 = genOddPalinRoots(11)
    target11.size should be(target11.toSet.size)
  }

  test("genEvenBigInt") {
    val target2 = genEvenPalinRoots(2)
    target2 should contain(BigInt("11"))
    target2 should contain(BigInt("22"))
    target2.size should be(2)

    val target4 = genEvenPalinRoots(4)
    target4 should contain(BigInt("1001"))
    target4 should contain(BigInt("2002"))
    target4 should contain(BigInt("1111"))
    target4 should not contain (BigInt("2112"))

    val target10 = genEvenPalinRoots(10)
    target10.size should be(target10.toSet.size)
  }

  test("allCombis") {
    val target1 = genAllFairAndSqureNums(1)
    val target2 = genAllFairAndSqureNums(2)
    assert(target1.toSet.subsetOf(target2.toSet))

    val target4 = genAllFairAndSqureNums(4)
    assert(target2.toSet.subsetOf(target4.toSet))

    val target20 = genAllFairAndSqureNums(20)
    assert(target4.toSet.subsetOf(target20.toSet))
  }
}