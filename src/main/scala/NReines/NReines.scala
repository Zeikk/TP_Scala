package NReines

class NReines(val n: Int) {


  // Question 1 :
  // étant donnée la position des reines précedentes, renvoye vrai si la colone est compatible
  def estOk(col: Int, reines: List[Int]): Boolean = ???

  // Question 2
  // calcule la liste des solutions
  lazy val solutions: Set[List[Int]] = ???

  // Question 3
  // Retourne le nombre de solutions
  lazy val nombreSolutions: Int = ???

  // question 4
  // transforme une solution en un String afichable
  def afficheSolution(solution: List[Int]): String = ???

  def afficheToutesSolutions(): Unit = for {
    (solution, i) <- solutions.zipWithIndex
  } println(s"Solution N°${i + 1} :\n " + afficheSolution(solution))


}
