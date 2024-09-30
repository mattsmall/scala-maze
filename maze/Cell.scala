package maze

class Cell(xIndex: Int, yIndex: Int) {

  require(xIndex >= 0)
  require(yIndex >= 0)

  val x: Int = xIndex
  val y: Int = yIndex
  var visited = false

  private var n: Boolean = false
  private var s: Boolean = false
  private var e: Boolean = false
  private var w: Boolean = false

  def isOpen(side: Side): Boolean = {
    side match {
      case N() => n
      case S() => s
      case E() => e
      case W() => w
      case _ => throw new IllegalArgumentException
    }
  }

  def open(side: Side) : Cell = {
    side match {
      case N() => n = true
      case S() => s = true
      case E() => e = true
      case W() => w = true
      case _ => throw new IllegalArgumentException
    }
    this
  }

  override def toString = {
    "(" + x + "," + y + ")" // + n + " " + e + " " + s + " " + w
  }

}

object Cell {
  val dirs = List(N(), E(), S(), W())
}

abstract class Side {
  def opposite(): Side
}
case class N extends Side {
  def opposite(): Side = S()
}
case class S extends Side {
  def opposite(): Side = N()
}
case class E extends Side {
  def opposite(): Side = W()
}
case class W extends Side {
  def opposite(): Side = E()
}
