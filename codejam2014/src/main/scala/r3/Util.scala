package r3

object Util {
  

  case class Grid[T](list: List[List[T]]) {
    def get(x: Int, y: Int): T = list(x)(y)

    def getColumns(x: Int): List[T] = list.map(_(x))

    def getRows(y: Int): List[T] = list(y)

    def rotateRight: Grid[T] = {
      val res = for (i <- 0 until list(0).length) yield getColumns(i)
      Grid(res.toList.map(_.reverse))
    }

    def flipVertical: Grid[T] = Grid(list.reverse)

    def flipHorizontal: Grid[T] = Grid(list.map(_.reverse))

    override def toString = list.map(_.mkString(" ")).mkString("\n")
  }

  implicit class StringWrapper(str: String) {
    def toIntList: List[Int] = str.split(" ").toList.map(_.toInt)
  }

  implicit class StringListWrapper(lines: List[String]) {

    // 문자열(lines)를 입력 받아 특정 타입의 Grid를 생성
    def toGrid[T](f: String => T): Grid[T] = Grid[T](lines.map(_.split(" ").toList.map(f)))

    // Int 타입의 Grid를 생성
    def toIntGrid: Grid[Int] = toGrid[Int](_.toInt)

    // float 타입의 Grid를 생성 
    def toFloatGrid: Grid[Float] = toGrid[Float](_.toFloat)

  }

  implicit class ListIntWrapper[T](list: List[List[T]]) {
    def toGrid: Grid[T] = Grid(list)
  }

}