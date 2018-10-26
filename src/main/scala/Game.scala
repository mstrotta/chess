package chess

case class Game() {
  val board = Board()
  val moveHistory: List[Move] = Nil
  val isCheckmate = false
}


