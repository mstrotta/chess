package chess

object main extends App {
  println("Chess Main")

  //val human = Player(true, Option("white"))
  //val engine = Player(false, Option("black"))
  //val game = new Game(human, engine)
  GameLoop.run()

}
