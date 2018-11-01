package chess

import scala.io.StdIn._

case class Game() {
  val board = new Board()
  val moveHistory: List[Move] = Nil
  val isCheckmate = false
  def isCheck = false
  def isLegalMove = ???
}

object GameLoop {
  private val game = Game()

  def run(): Unit = {
    var move: Option[Move] = None
    do {
      print("\033[2J")
      println(game.board)
      move = promptMove()
      move foreach( game.board.move(_) )
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

object Main extends App {
  //def main(args[])
  GameLoop.run
}
