val x: Int = 5
println(x)

val matrix = Array.ofDim[Int](2,2)

val board = Array.ofDim[Option[String]](8, 8)

def f(i: Int, j: Int): Char = {
  (i, j) match {
    case (1, _) => 'P'

    case _ => ' '
  }
}

val board2 = Array.tabulate[Char](8,8)(f)


