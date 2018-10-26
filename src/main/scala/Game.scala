package chess

case class Game() {
  val board = Board()
  val moveHistory: List[Move] = Nil
  val isCheckmate = false
}

object Move {
  private val Coords = "^?(\\d)(\\d)? ?(\\d)(\\d)?".r

  def apply(move: String): Move = {
    move match {
      case Coords(i, j, m, n) => Move(Array(i.toInt, j.toInt, m.toInt, n.toInt))
      case _ =>
        throw new IllegalArgumentException(s"Move format unrecognized: $move")
    }
  }

  def isValid(move: String): Boolean = {
    try {
      Move(move)
      true
    } catch {
      case _: IllegalArgumentException => false
      case e: Throwable                => throw e
      case _                           => true
    }
  }
}

case class Move(coords: Array[Int]) {}
