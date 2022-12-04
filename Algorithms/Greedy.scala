package object Greedy{
  def greedyFractionalKnapsack[A, B](l: List[A], max: B, isFeasible: (A, B) => Double, compare: (A, A) => Boolean, update: (A, B) => B, empty: B => Boolean): List[(A, Double)] = {
    val auxQ = scala.collection.mutable.Queue(l.sortWith(compare): _*)
    var sol = List(): List[(A, Double)]
    var auxMax = max
    while (!empty(auxMax) && l.nonEmpty) {
      val next = auxQ.dequeue()
      sol = sol :+ (next, isFeasible(next, auxMax))
      auxMax = update(next, auxMax)
    }
    sol
  }
}
