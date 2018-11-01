package chess

sealed trait Color
case object White extends Color
case object Black extends Color
case object NoColor extends Color

object BoardSquare {
  def apply(row: Int, col: Int): BoardSquare = {
    require(0 <= row && row < 8 && 0 <= col && col < 8,
            "BoardSquare must be in interval [0,8]")
    new BoardSquare(row, col)
  }
}

/**
* BoardSquare wraps an i,j tuple (note row and col are 0-7 with origin at top-left)
  * @param row 0 to 7
  * @param col 0 to 7
  */
class BoardSquare(row: Int, col: Int) {
  val coord = Array(row, col)
  def apply(i: Int) = coord(i)

  /**
    * Add an integer tuple (di, dj) to a square
    * @param dij delta ij in screen coordinates
    * @return Some() if on board
    */
    try {
      Some(new BoardSquare(row + dij._1, col + dij._2))
    } catch {
      case _: IllegalArgumentException => None
      case e: Throwable                => throw e
    }
}

/**
* Board contains underlying 2D-array of Pieces. It doesn't know the rules of chess
  */
class Board() {
  val squares: Array[Array[Piece]] = initializePieces()
  //val castled: List[Color] = Nil

  // Three-fold repetition
  //   We should have a counting hash map of the set of all legal moves for each player to know if the game is a draw

  // En-passant
  //   We must know the previous move in order to know if en-passant capture is a legal move

  def apply(sq: BoardSquare): Piece = getPiece(sq)

  def move(m: Move): Unit = {
    squares(m.to(0))(m.to(1)) = squares(m.from(0))(m.from(1))
    squares(m.from(0))(m.from(1)) = Empty(BoardSquare(m.from(0), m.from(1)))
  }

  def isOccupied(sq: BoardSquare): Boolean = getPiece(sq) match {
    case Empty(_) => false
    case _        => true
  }

  def getPiece(sq: BoardSquare): Piece = squares(sq(0))(sq(1))

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
      Array.tabulate(8)(i => Empty(BoardSquare(5, i))),
      Array.tabulate(8)(i => Empty(BoardSquare(4, i))),
      Array.tabulate(8)(i => Empty(BoardSquare(3, i))),
      Array.tabulate(8)(i => Empty(BoardSquare(2, i))),
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

  override def toString: String = {
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
      case _: IllegalArgumentException =>  println("Invalid"); false
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
case class Move(from: BoardSquare, to: BoardSquare) {}
sealed trait MoveList {
  def toList: List[Move]
}
final case class MoveSet private (_moves: Move*) extends MoveList {
  def toList: List[Move] = _moves.toList
}
final case class MovePath private (_moves: Move*) extends MoveList {
  def takeWhile(p: Move => Boolean): List[Move] = _moves.toList.takeWhile(p)
  def toList: List[Move] = _moves.toList
}

/**
* A Piece knows its color and square, and it knows how it moves and captures
  */
sealed trait Piece {
  val color: Color
  val square: BoardSquare
  def getMotion: List[List[BoardSquare]]
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
final case class Empty(square: BoardSquare) extends Piece {
  val color: Color = NoColor
  def getMotion: List[List[BoardSquare]] = Nil
}
final class Pawn(val color: Color, val square: BoardSquare) extends Piece {
  val dir: Int = if (color == White) 1 else -1
  val homeRow: Int = if (color == White) 1 else 6
  def getMotion: List[List[BoardSquare]] = {
    if (square(0) == homeRow)
      List(
        List(square + (dir, 0)).flatten,
        (1 to 2).flatMap(i => square + (i * dir, 0)).toList
      )
    else
      List(List(square + (dir, 0)).flatten)
  }
  def getCaptureMotion = List(square + (dir, -1), square + (dir, 1))
}
final class King(val color: Color, val square: BoardSquare) extends Piece {
  def getMotion: List[List[BoardSquare]] =
    List(
      square + (0, 1),
      square + (0, -1),
      square + (1, 1),
      square + (1, -1),
      square + (1, 0),
      square + (-1, 0),
      square + (-1, -1),
      square + (-1, 1)
    ).flatten.map(List(_))
}
final class Knight(val color: Color, val square: BoardSquare) extends Piece {
  def getMotion: List[List[BoardSquare]] =
    List(
      square + (1, 2),
      square + (1, -2),
      square + (2, 1),
      square + (2, -1),
      square + (-1, 2),
      square + (-1, -2),
      square + (-2, 1),
      square + (-2, -1)
    ).flatten.map(List(_))
}
final class Queen(val color: Color, val square: BoardSquare) extends Piece {
  def getMotion: List[List[BoardSquare]] = {
    val r = new Rook(color, square)
    val b = new Bishop(color, square)
    r.getMotion ++ b.getMotion
  }
}
final class Rook(val color: Color, val square: BoardSquare) extends Piece {

  def getMotion: List[List[BoardSquare]] = {
    val (i0, j0) = (square(0), square(1))
    for (dij <- List(1, -1, 0, 0).zip(List(0, 0, 1, -1))) yield {
      val (di, dj) = dij
      val path: Seq[BoardSquare] = for {
        i <- Range(i0, i0 + 8 * di, di) if 0 <= i && i < 8
        j <- Range(j0, j0 + 8 * dj, dj) if 0 <= j && j < 8
      } yield BoardSquare(i, j)
      path.toList
    }
  }
}
final class Bishop(val color: Color, val square: BoardSquare) extends Piece {
  def getMotion: List[List[BoardSquare]] = {
    val (i0, j0) = (square(0), square(1))
    for {
      di <- List(-1, 1)
      dj <- List(-1, 1)
    } yield {
      val path: Seq[BoardSquare] = for {
        i <- Range(i0, i0 + 8 * di, di) if 0 <= i && i < 8
        j <- Range(j0, j0 + 8 * dj, dj) if 0 <= j && j < 8
      } yield BoardSquare(i, j)
      path.toList
    }
  }
}
