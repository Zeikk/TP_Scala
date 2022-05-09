package NReines

class NReines(val n: Int) {


  // Question 1 :
  // étant donnée la position des reines précedentes, renvoye vrai si la colone est compatible
  def estOk(col: Int, reines: List[Int]): Boolean = {

    val row = n - reines.length - 1

    def isDiagonal(reines: List[Int]) : Boolean = {

      if(reines.isEmpty)
        false
      else {
        val rowOldCol = n - reines.length
        math.abs(rowOldCol - row) == math.abs(col - reines.head) || isDiagonal(reines.tail)
      }

    }

    !reines.contains(col) && !isDiagonal(reines)
  }

  // Question 2
  // calcule la liste des solutions
  lazy val solutions: Set[List[Int]] = {

    def rec_solution(result : Set[List[Int]], reines : Int) : Set[List[Int]] =  {

      if(reines == n) {
        result
      } else {
        val set = for{
          i <- 0 until n
          list <- result
          if estOk(i, list)
        } yield i :: list

        rec_solution(set.toSet, reines + 1)
      }

    }

    val set = for{
      i <- 0 until n
    } yield i :: Nil

    rec_solution(set.toSet, 1)

  }

  // Question 3
  // Retourne le nombre de solutions
  lazy val nombreSolutions: Int = solutions.size

  // question 4
  // transforme une solution en un String afichable
  def afficheSolution(solution: List[Int]): String = {

    val res : List[String] = for {
      col <- solution
    } yield "O " * col + "X " + "O " * (n - col - 1)

    res.map((elem) => elem.trim()).mkString("\n")
  }

  def afficheToutesSolutions(): Unit = for {
    (solution, i) <- solutions.zipWithIndex
  } println(s"Solution N°${i + 1} :\n " + afficheSolution(solution))

}
