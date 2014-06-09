package qr

import org.scalatest.FunSuite
import org.scalatest.Matchers
import qr.Treasure._

class KeyHolderTest extends FunSuite with Matchers {

  test("Keyholder - operator") {
    val keyholder = new KeyHolder(List(1, 2, 3))
    val rest: KeyHolder = keyholder - 2
    rest should be(KeyHolder(List(1, 3)))
  }

  test("Find match chest") {
    val keyholder = new KeyHolder(List(1, 2, 3))
    val chests = List(Chest(1, 4, Nil), Chest(2, 3, Nil), Chest(3, 2, Nil))
    keyholder findMatchType chests should be(Some(Chest(2, 3, Nil)))
  }

  test("Keyholder has a key that match one of chest list") {
  }

  test("KeyHolder do not have a key that match chest in the list") {
    val empty = Nil
    val keyHolder = KeyHolder(List(3, 4))
    val chests = List(Chest(1, 1, empty), Chest(2, 2, empty))
    val res = keyHolder findMatchType (chests)
    res should be(None)
  }
}

class MapTest extends FunSuite with Matchers {
  test("Unlock one chest") {
    val keyholder = KeyHolder(List(1, 3, 4))
    val chests = List(Chest(1, 1, Nil), Chest(2, 2, Nil))
    val map = Map(KeyHolder(Nil), Nil)

    //    map.tryUnlock(keyholder, chests, Nil).get should be(KeyHolder(List(3, 4)), List(Chest(2, 2, Nil)), List(1))
  }

  test("Unlock no chest") {
    val keyholder = KeyHolder(List(2, 4, 6))
    val chests = List(Chest(1, 1, Nil), Chest(2, 3, Nil), Chest(3, 5, Nil))
    val map = Map(KeyHolder(Nil), Nil)

    //    map.tryUnlock(keyholder, chests, Nil) should be(None)
  }
}

class TreasureTest extends FunSuite with Matchers {

  // TODO: keyHolder에 키를 넣는 경우, 순서대로 들어가는지 확
  test("Chest ordering") {
    val chests = List(Chest(1, 2, Nil), Chest(2, 1, Nil), Chest(3, 4, Nil), Chest(4, 3, Nil))
    chests.sortBy(_.t) should be(List(Chest(2, 1, Nil), Chest(1, 2, Nil), Chest(4, 3, Nil), Chest(3, 4, Nil)))
  }

  test("Possible case 1") {
    val keyHolder = KeyHolder(List(1, 2, 3))
    val chests = List(Chest(1, 1, Nil), Chest(2, 2, Nil), Chest(3, 3, List(4)), Chest(4, 4, Nil))
    Map(keyHolder, chests).solve.get should be(List(1, 2, 3, 4))
  }

  test("Possible case 2") {
    val keyHolder = KeyHolder(List(1))
    val chests = List(Chest(1, 1, List(2)), Chest(2, 2, List(3)), Chest(3, 3, Nil))
    Map(keyHolder, chests).solve.get should be(List(1, 2, 3))
  }

  test("Possible case 3 (example)") {
    val keyHolder = KeyHolder(List(1))
    val chests = List(Chest(1, 1, Nil), Chest(2, 1, List(1, 3)), Chest(3, 2, Nil), Chest(4, 3, List(1, 2)))
    Map(keyHolder, chests).solve.get should be(List(2, 1, 4, 3))
  }

  test("Impossible case 1") {
    val keyHolder = KeyHolder(List(1, 2, 3))
    val chests = List(Chest(1, 1, Nil), Chest(2, 2, Nil), Chest(3, 4, Nil))
    Map(keyHolder, chests).solve should be(None)
  }

  test("Impossible case 2") {
    val keyHolder = KeyHolder(List(4))
    val chests = List(Chest(1, 1, List(2)), Chest(2, 2, List(3)), Chest(3, 3, Nil))
    Map(keyHolder, chests).solve should be(None)
  }
}