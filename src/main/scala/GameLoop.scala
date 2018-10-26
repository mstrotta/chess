package chess

import scala.io.StdIn._

object GameLoop {
  private val game = Game()

  def run(): Unit = {
    var move: Option[Move] = None
    do {
      println(game.board)
      move = promptMove()
      move foreach { m =>
        game.board.move((m.coords(0), m.coords(1)), (m.coords(2), m.coords(3)))
      }
    } while (move.nonEmpty && !game.isCheckmate)
  }

  def promptMove(): Option[Move] = {
    var s = ""
    do {
      s = readLine("Enter move (e.g. `ij mn`): ").trim
      println(s"Gave move: $s")
    } while (!isQuit(s) && !Move.isValid(s))
    if (Move.isValid(s)) Some(Move(s)) else None
  }

  def isQuit(s: String): Boolean = s.headOption.exists(_.toUpper == 'Q')
}
