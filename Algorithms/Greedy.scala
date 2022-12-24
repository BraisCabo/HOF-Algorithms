package object Greedy{
  def greedyFractionalKnapsack[A, B](l: List[A], max: B, isFeasible: (A, B) => Double, compare: (A, A) => Boolean,
                                     update: (A, B) => B, empty: B => Boolean): List[(A, Double)] = {
    List(l.sortWith(compare): _*).foldLeft((List(): List[(A, Double)], max)){
      case ((sol, aux), _)  if empty(aux) => (sol, aux)
      case ((sol, aux), e) => (sol :+ (e, isFeasible(e, aux)), update(e, aux))
    }._1
  }
}
