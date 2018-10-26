import org.scalatest._
import chess.Board

class BoardSpec extends FlatSpec {
  "Board" should "be 8x8" in {
    val b = Board()
    assert(b.squares.lengthCompare(8) == 0, s"Only ${b.squares.length} rows!")
    for (i <- 0 until 8) {
      assert(b.squares(i).lengthCompare(8) == 0, s"Row $i had size ${b.squares(i).length}")
    }
  }

  "New boards" should "be set up correctly" in {
    val b: Board = Board()
    println(b)
    println()
  }
  "Pieces" should "move to correct positions" in {
    val b = Board()
    b.move((6,4), (4,4))
    println(b)
    println()
    b.move((1,4), (3,4))
    println(b)
  }
}
