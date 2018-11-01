package chess

import org.scalatest._

class BoardSpec extends FlatSpec {
  "Board" should "be 8x8" in {
    val b = new Board()
    assert(b.squares.lengthCompare(8) == 0, s"Only ${b.squares.length} rows!")
    for (i <- 0 until 8) {
      assert(b.squares(i).lengthCompare(8) == 0, s"Row $i had size ${b.squares(i).length}")
    }
  }

  "New boards" should "be set up correctly" in {
    val b: Board = new Board()
    println(b)
    println()
  }
  "Pieces" should "move to correct positions" in {
    val b = new Board()
    b.move(Move(BoardSquare(6,4), BoardSquare(4,4)))
    println(b)
    println()
    b.move(Move(BoardSquare(1,4), BoardSquare(3,4)))
    println(b)
  }
}
