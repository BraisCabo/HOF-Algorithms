package Voraces
@main def greedyFractionalKnapsack(): Unit = {
  case class Card(Risk: Int, Profit: Int)

  def isFeasible(a: Card, i: Int) : Boolean = {
    return (i - a.Risk) >= 0
  }

  def apply(l: List[Card], max: Int): List[(Card, Double)] = {
    val auxQ = scala.collection.mutable.Queue(l.sortWith((a, b) => b.Profit.toDouble / (
      if (b.Risk == 0) 1
      else b.Risk
      ) < a.Profit.toDouble / (
      if (a.Risk == 0) 1
      else a.Risk
      )): _*)
    var sol = List(): List[(Card, Double)]
    var auxMax = max
    while (auxMax > 0 && auxQ.nonEmpty) {
      val next = auxQ.dequeue()
      if (isFeasible(next, auxMax)){
        sol = sol :+ (next, 1.0)
      }else{
        sol = sol :+ (next, (next.Risk - (next.Risk - auxMax)).toDouble / 100)
      }
      auxMax = auxMax - next.Risk
    }
    sol
  }

  val solucion = apply(List(Card(0, 100), Card(100, 10), Card(70, 100)),
    160)
  print(solucion)
}