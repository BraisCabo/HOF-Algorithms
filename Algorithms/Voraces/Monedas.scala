package Voraces

@main def greedyCoins(): Unit = {
  def isFeasible(a: Int, i: Int) : Boolean = {
    return (i - a) >= 0
  }

  def apply(l: List[Int], max: Int): List[(Int, Int)] = {
    val auxQ = scala.collection.mutable.Queue(l.sortWith((a, b) =>  a > b): _*)
    var sol = List(): List[(Int, Int)]
    var auxMax = max
    while (auxMax > 0 && auxQ.nonEmpty) {
      val next = auxQ.dequeue()
      if (isFeasible(next, auxMax)){
        sol = sol :+ (next, auxMax/next)
        auxMax = auxMax - next * auxMax/next
      }
    }
    sol
  }

  val solucion = apply(List(1, 2, 5, 10, 20, 50, 100, 200), 40)
  print(solucion)
}
