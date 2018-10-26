package chess

case class Board() {
  val squares: Array[Array[Char]] = initializePieces()

  def move(src: (Int, Int), dest: (Int, Int)): Unit = {
    assert(0 <= src._1 && src._1 < 8, "Not on board!")
    assert(0 <= src._1 && src._1 < 8, "Not on board!")
    assert(0 <= dest._2 && dest._2 < 8, "Not on board!")
    assert(0 <= dest._2 && dest._2 < 8, "Not on board!")
    val piece = squares(src._1)(src._2)
    assert(piece != ' ', "Given square is empty")
    squares(src._1)(src._2) = ' '
    squares(dest._1)(dest._2) = piece
  }

  private def initializePieces(): Array[Array[Char]] = {
    Array[Array[Char]](
      Array('r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'),
      Array.fill(8)('p'),
      Array.fill(8)(' '),
      Array.fill(8)(' '),
      Array.fill(8)(' '),
      Array.fill(8)(' '),
      Array.fill(8)('P'),
      Array('R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R')
    )
  }

  override def toString(): String = {
    squares.map(line => line.mkString(" ")).mkString("\n")
  }
}
