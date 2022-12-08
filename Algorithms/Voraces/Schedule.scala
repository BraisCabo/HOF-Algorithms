package Voraces

@main def greedySchedule(): Unit = {
  case class Jobs(Limite : Int, Profit: Int, Id: String)


  def isFeasible(jobs: Array[(Jobs, Int)], i : Int) : Boolean = {
   return (jobs(i) == null)
  }

  def apply(l: List[Jobs], max: Int): List[(Jobs, Int)] = {
    val auxQ = scala.collection.mutable.Queue(l.sortWith((a, b) =>  a.Profit > b.Profit): _*)
    var sol = new Array[(Jobs, Int)](max)
    var auxMax = max
    while (auxMax > 0 && auxQ.nonEmpty) {
      val next = auxQ.dequeue()
      var auxDeadline = next.Limite
      while (auxDeadline > -1){
        if (isFeasible(sol, auxDeadline)){
          sol(auxDeadline) = (next, auxDeadline)
          auxDeadline = - 1
          auxMax = auxMax - 1
        }else{
          auxDeadline -= 1
        }
      }

    }
    sol.toList
  }

  val solucion = apply(List(Jobs(2, 50, "Cortafuegos"), Jobs(1, 10, "Algoritmos"), Jobs(2, 15, "Testing"),Jobs(1, 30, "Presentacion") ), 3)
  print(solucion)
}
