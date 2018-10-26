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
      case Coords(i,j,m,n) => Move(s"$i$j$m$n")
      case _ => throw new IllegalArgumentException(s"Move format unrecognized: $move")
    }
  }
}

case class Move(move: String) {
}
