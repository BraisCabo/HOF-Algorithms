package Voraces

@main def greedyWaitingTime(): Unit = {
  case class Patient(Time: Int, Name: String)

  def apply(l: List[Patient]): List[(Patient)] = {
    val auxQ = scala.collection.mutable.Queue(l.sortWith((a, b) =>  a.Time < b.Time): _*)
    var sol = List(): List[Patient]
    while (auxQ.nonEmpty) {
      val next = auxQ.dequeue()
        sol = sol :+ next
    }
    sol
  }

  val solucion = apply(List(Patient(100, "Carlos"), Patient(20, "Juan"), Patient(99, "Jose")))
  print(solucion)
}
