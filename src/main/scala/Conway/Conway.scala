package Conway

class Conway(init: Int = 1) {

  // renvoie le rang suivant : List(1, 1) --> List(2, 1)
  def lire(rang : List[Int]) : List[Int] = {

    def matcher(rang: List[Int], nbOccurence: Int) : List[Int] = rang match {
      case first :: Nil => {
        nbOccurence :: first :: Nil
      }
      case first :: tail => {
        if(first == tail.head)
          matcher(tail, nbOccurence + 1)
        else
          nbOccurence :: first  :: matcher(tail, 1)
      }
    }

    matcher(rang, 1)
  }

  def create_rangs(init : List[Int]) : LazyList[List[Int]] = {
    lire(init) #:: create_rangs(lire(init))
  }

  // la suite infinie de tout les rangs
  val rangs : LazyList[List[Int]] = create_rangs(List(init))

  //renvoie le rang sous forme de chaine de caractère
  // attention : rang commence à 1
  def apply(rang: Int): String = {

    def matcher(rang: Int) : List[Int] = rang match {
      case 1 => List(init)
      case _ => lire(matcher(rang - 1))
    }

    matcher(rang).mkString(" ")
  }

}
