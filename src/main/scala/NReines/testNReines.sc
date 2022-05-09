import NReines.NReines

val quatreReines = new NReines(4)

"""
quatreReines.solutions

quatreReines.nombreSolutions

quatreReines.afficheToutesSolutions
"""

val n = 4
def rec_solution(result : Set[List[Int]], reines : Int) : Set[List[Int]] =  {

  if(reines == 4) {
    result
  } else {
    val set = for{
      i <- 0 to (n - 1)
      list <- result
      if quatreReines.estOk(i, list)
    } yield i :: list

    rec_solution(set.toSet, reines + 1)
  }

}

rec_solution(Set(List(0), List(1), List(2), List(3)), 1)
