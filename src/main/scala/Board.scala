package chess

import scala.util.Try

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

  def coordToSquare() = ???

  //def toRow(i: Int) = 8 - i
  //def toCol(i: Int) = 1 + i

  def alphaToCoord(s: String): Option[(Int, Int)] = {
    def toNum(c: Char) = c.toLower.toInt - 'a'
    val p = "([a-h][A-H])(\\d)".r

    s match {
      case p(col, row) => Try(toNum(row.head), col.toInt).toOption
      case _ => None
    }
  }
}

case class Move(coords: Array[Int]) {}