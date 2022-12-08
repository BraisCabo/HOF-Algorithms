package Voraces

@main def greedyWaitingTime(): Unit = {
  case class Patient(Time: Int, Name: String)

  def isFeasible(a: Patient, i: Int) : Boolean = {
    return (i - a.Time) >= 0
  }

  def apply(l: List[Patient], max: Int): List[(Patient)] = {
    val auxQ = scala.collection.mutable.Queue(l.sortWith((a, b) =>  a.Time < b.Time): _*)
    var sol = List(): List[Patient]
    var auxMax = max
    while (auxMax > 0 && auxQ.nonEmpty) {
      val next = auxQ.dequeue()
      if (isFeasible(next, auxMax)){
        sol = sol :+ next
      }
      auxMax = auxMax - next.Time
    }
    sol
  }

  val solucion = apply(List(Patient(100, "Carlos"), Patient(20, "Juan"), Patient(99, "Jose")),
    160)
  print(solucion)
}
