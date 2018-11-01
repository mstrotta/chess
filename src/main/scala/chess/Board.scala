package chess

sealed trait Color
case object White extends Color
case object Black extends Color
case object NoColor extends Color

object BoardSquare {
  def apply(row: Int, col: Int) = {
    require(0 <= row && row < 8 && 0 <= col && col < 8,
            "BoardSquare must be in interval [0,8]")
    new BoardSquare(row, col)
  }
}
class BoardSquare(row: Int, col: Int) {
  // note row and col are 0-7 with origin at top-left
  val coord = Array(row, col)
  def apply(i: Int) = coord(i)

  /**
    * Add an integer tuple (di, dj) to a square
    * @param dij delta ij in screen coordinates
    * @return Some() if on board
    */
  def +(dij: (Int, Int)): Option[BoardSquare] =
    try {
      Some(new BoardSquare(row + dij._1, col + dij._2))
    } catch {
      case _: IllegalArgumentException => None
      case e: Throwable                => throw e
    }
}

class Board() {
  val squares: Array[Array[Piece]] = initializePieces

  def move(m: Move): Unit = {
    squares(m.to(0))(m.to(1)) = squares(m.from(0))(m.from(1))
    squares(m.from(0))(m.from(1)) = new Empty(BoardSquare(m.from(0), m.from(1)))
  }

  private def initializePieces(): Array[Array[Piece]] = {
    Array(
      Array(
        new Rook(Black, BoardSquare(7, 0)),
        new Knight(Black, BoardSquare(7, 1)),
        new Bishop(Black, BoardSquare(7, 2)),
        new Queen(Black, BoardSquare(7, 3)),
        new King(Black, BoardSquare(7, 4)),
        new Bishop(Black, BoardSquare(7, 5)),
        new Knight(Black, BoardSquare(7, 6)),
        new Rook(Black, BoardSquare(7, 7))
      ),
      Array.tabulate[Piece](8)(i => new Pawn(Black, BoardSquare(6, i))),
      Array.tabulate(8)(i => new Empty(BoardSquare(5, i))),
      Array.tabulate(8)(i => new Empty(BoardSquare(4, i))),
      Array.tabulate(8)(i => new Empty(BoardSquare(3, i))),
      Array.tabulate(8)(i => new Empty(BoardSquare(2, i))),
      Array.tabulate[Piece](8)(i => new Pawn(White, BoardSquare(1, i))),
      Array(
        new Rook(White, BoardSquare(7, 0)),
        new Knight(White, BoardSquare(7, 1)),
        new Bishop(White, BoardSquare(7, 2)),
        new Queen(White, BoardSquare(7, 3)),
        new King(White, BoardSquare(7, 4)),
        new Bishop(White, BoardSquare(7, 5)),
        new Knight(White, BoardSquare(7, 6)),
        new Rook(White, BoardSquare(7, 7))
      )
    )
  }

  override def toString(): String = {
    squares.map(line => line.mkString(" ")).mkString("\n")
  }
}

object Move {
  private val coords = "(\\d)(\\d) ?(\\d)(\\d)".r
  private val square = "([a-hA-H][1-8]) ?([a-hA-H][1-8])".r

  def apply(move: String): Move = {
    move match {
      case coords(i, j, m, n) =>
        Move(BoardSquare(i.toInt, j.toInt), BoardSquare(m.toInt, n.toInt))
      case square(src, dest) =>
        Move(alphaToSquare(src).get, alphaToSquare(dest).get)
      case _ =>
        throw new IllegalArgumentException(s"Move format unrecognized: $move")
    }
  }

  def isValid(move: String): Boolean = {
    try {
      Move(move)
      true
    } catch {
      case _: IllegalArgumentException => { println("Invalid"); false }
      case e: Throwable                => throw e
    }
  }

  def alphaToSquare(s: String): Option[BoardSquare] = {
    // Returns coords in
    def toNum(c: Char) = c.toLower.toInt - 'a'
    val regex = "([a-h])(\\d)".r

    s.toLowerCase match {
      case regex(col, row) => Some(BoardSquare(8 - row.toInt, toNum(col.head)))
      case _               => None
    }
  }
}

case class Move(val from: BoardSquare, val to: BoardSquare) {}

sealed trait Piece {
  val color: Color
  val square: BoardSquare
  def getMotion: List[Option[BoardSquare]]
  override def toString: String = {
    val c = this match {
      case _: Pawn   => '\u2659'
      case _: Knight => '\u2658'
      case _: Bishop => '\u2657'
      case _: Rook   => '\u2656'
      case _: Queen  => '\u2655'
      case _: King   => '\u2654'
      case _         => ' '
    }
    val d: Int = if (color == White) 6 else 0
    (c + d).toChar.toString
  }
}
final class Empty(val square: BoardSquare) extends Piece {
  val color = NoColor
  def getMotion: List[Option[BoardSquare]] = Nil
}
final class Pawn(val color: Color, val square: BoardSquare) extends Piece {
  val dir: Int = if (color == White) 1 else -1
  val homeRow: Int = if (color == White) 1 else 6
  def getMotion = {
    if (square(0) == homeRow)
      List(square + (dir, 0), square + (2 * dir, 0))
    else
      List(square + (dir, 0))
  }
  def getCaptureMotion = List(square + (dir, -1), square + (dir, 1))
}
final class King(val color: Color, val square: BoardSquare) extends Piece {
  def getMotion = List(
    square + (0, 1),
    square + (0, -1),
    square + (1, 1),
    square + (1, -1),
    square + (1, 0),
    square + (-1, 0),
    square + (-1, -1),
    square + (-1, 1)
  )
}
final class Knight(val color: Color, val square: BoardSquare) extends Piece {
  def getMotion =
    List(
      square + (1, 2),
      square + (1, -2),
      square + (2, 1),
      square + (2, -1),
      square + (-1, 2),
      square + (-1, -2),
      square + (-2, 1),
      square + (-2, -1)
    )
}
final class Queen(val color: Color, val square: BoardSquare) extends Piece {
  def getMotion = {
    val r = new Rook(color, square)
    val b = new Bishop(color, square)
    r.getMotion ++ b.getMotion
  }
}
final class Rook(val color: Color, val square: BoardSquare) extends Piece {
  def getMotion = {
    val i0 = square(0)
    val j0 = square(1)
    for {
      dij <- List(1, -1, 0, 0)
        .zip(List(0, 0, 1, -1))
      di = dij._1
      dj = dij._2
      i <- Range(i0, i0 + 8 * di, di) if 0 <= i && i < 8
      j <- Range(j0, j0 + 8 * dj, dj) if 0 <= j && j < 8
    } yield Some(new BoardSquare(i, j))
  }
}
final class Bishop(val color: Color, val square: BoardSquare) extends Piece {
  def getMotion = {
    val i0 = square(0)
    val j0 = square(1)
    for {
      di <- List(-1, 1)
      dj <- List(-1, 1)
      i <- Range(i0, i0 + 8 * di, di) if 0 <= i && i < 8
      j <- Range(j0, j0 + 8 * dj, dj) if 0 <= j && j < 8
    } yield Some(new BoardSquare(i, j))
  }
}
