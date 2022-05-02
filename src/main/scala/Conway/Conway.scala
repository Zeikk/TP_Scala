package Conway

class Conway(init: Int = 1) {

  // renvoie le rang suivant : List(1, 1) --> List(2, 1)
  def lire(rang : List[Int]) : List[Int] = {

    def inc_lire(rang: List[Int], result: List[Int], nbOccurence: Int, current: Int): List[Int] = {
      if(rang.isEmpty) {
        result :+ nbOccurence :+ current
      } else if (rang.head != current) {
        inc_lire(rang.tail, result :+ nbOccurence :+ current, 1, rang.head)
      } else {
        inc_lire(rang.tail, result , nbOccurence + 1, rang.head)
      }
    }

    inc_lire(rang.tail, List(), 1, rang.head)
  }

  // la suite infinie de tout les rangs
  val rangs : LazyList[List[Int]] = List(lire(lire(List(1)))).to(LazyList)


  //renvoie le rang sous forme de chaine de caractère
  // attention : rang commence à 1
  def apply(rang: Int): String = {
    def inc_apply(rang: Int, result: List[Int]) : List[Int] = {
      if(rang > 0)
        inc_apply(rang - 1, lire(result))
      else result
    }
    inc_apply(rang- 1, List(init)).mkString(" ")
  }

}
