import Greedy.greedyFractionalKnapsack
@main def greedyFractionalKnapsackTest(): Unit = {
  case class Card(Risk: Int, Profit: Int)

  greedyFractionalKnapsack[Card, Int](
    List(Card(0, 100), Card(100, 10), Card(70, 100)),
    160,
    (a, b) => if ((b - a.Risk) > 0) 1.0
    else (a.Risk - (a.Risk - b)).toDouble / 100,
    (a, b) => b.Profit.toDouble / (
      if (b.Risk == 0) 1
      else b.Risk
      ) < a.Profit.toDouble / (
      if (a.Risk == 0) 1
      else a.Risk
      ),
    (a, b) => b - a.Risk,
    _ <= 0
  )
}