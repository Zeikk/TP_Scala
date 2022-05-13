import Logique._

val interp: Interpretation = Map("A" -> true, "B" -> true, "C" -> false, "D" -> true)
val expr1: Expression = Ou(Proposition("A"), Proposition("C"))

val e1 = Equivalent(Proposition("R"), Et(Proposition("P"), Et(Proposition("A"), Proposition("S"))))
val e2 = Proposition("P")
val e3 = Non(Proposition("R"))
val th = Ou(Non(Proposition("A")), Non(Proposition("S")))

expr1.toString.split('(')

expr1.getClass == Logique.Ou.getClass

evaluation(expr1)(interp)

val e = Set("A", "B", "P")
def gray(e: Set[String]): List[Map[String, Boolean]] = {
  /*
  Algorithm:
   1. Start with a gray code sequence of n-1
   2. Reverse the list
   3. Concatenate original and reverse lists
   4. Prepend original list with 0 and reverse list with 1
   */
  if (e.size == 1) {
    List(Map(e.head -> false), Map(e.head -> true))
  } else {
    val original = gray(e.tail)
    val reversed = original.reverse
    original.map(Map(e.head -> false) ++ _) ++ reversed.map(Map(e.head -> true) ++ _)
  }
}

gray(e)

val propositions = ensembleProposition(Set(th))
listeInterpretation(propositions)

val tab1 = new Tableau(List(e1, e2, e3), th)

val interpretations = listeInterpretation(tab1.propositions)
interpretations.head.values.toList
