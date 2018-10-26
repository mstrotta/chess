package chess

import scala.io.StdIn._

object GameLoop {
  private val input: Option[String] = None
  private val game = Game()

  def run() {
    do {
      input = getMove()
    } while (!isQuit(input) && !)
  }

  def getMove(): String = {

    do {
      val s = readLine("Enter move (e.g. `ij mn`): ")
      println(s"Gave $s")
    } while
  }
  def isQuit(command: String): Boolean = {
    command.headOption.exists(_.toUpper != 'Q')
  }
}
