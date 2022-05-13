package object Logique {

  // une interprétation associe une valeur de vérité à une proposition
  // Par exemple, on pourrait avoir l'interprétation ssuivante :
  // Map ( "A" -> true , "B" -> true, "C" -> false, "D" -> true)
  type Interpretation = Map[String, Boolean]

  // une base de connaissance est une liste d'expressions
  type BaseConnaissance = List[Expression]

  // un théorème est une expression
  type Theoreme = Expression

  // une expression logique se compose de constantes, de propositions,
  // et des opérateurs et, ou, non, ainsi que l'implication et l'équivalence
  sealed trait Expression

  sealed trait OperateurBinaire extends Expression {
    val expGauche: Expression
    val expDroite: Expression
  }

  case class Constante(valeur: Boolean) extends Expression

  case class Proposition(nom: String) extends Expression

  case class Non(exp: Expression) extends Expression

  case class Ou(expGauche: Expression, expDroite: Expression) extends OperateurBinaire

  case class Et(expGauche: Expression, expDroite: Expression) extends OperateurBinaire

  case class Implique(expGauche: Expression, expDroite: Expression) extends OperateurBinaire // (vrai => faux) est faux, tout le reste est vrai

  case class Equivalent(expGauche: Expression, expDroite: Expression) extends OperateurBinaire

  // Question 1 :
  // calculer la valeur de vérite d'une expression, étant donnée une interprétation
  def evaluation(expr: Expression)(implicit interp: Interpretation): Boolean = {
    def matcher(expression: Expression) : Boolean = expression match {
      case Ou(_, _) => {
        val ou_exp = expression.asInstanceOf[Ou]
        matcher(ou_exp.expGauche) || matcher(ou_exp.expDroite)
      }
      case Non(_) => {
        val non_exp = expression.asInstanceOf[Non]
        ! matcher(non_exp.exp)
      }
      case Et(_, _) => {
        val et_exp = expression.asInstanceOf[Et]
        matcher(et_exp.expGauche) && matcher(et_exp.expDroite)
      }
      case Implique(_, _) => {
        val imp_exp = expression.asInstanceOf[Implique]
        val imp1 = matcher(imp_exp.expGauche)
        val imp2 = matcher(imp_exp.expDroite)
        if(imp1 && !imp2) false else true
      }
      case Equivalent(_, _) => {
        val eq_exp = expression.asInstanceOf[Equivalent]
        matcher(Implique(eq_exp.expGauche, eq_exp.expDroite)) && matcher(Implique(eq_exp.expDroite, eq_exp.expGauche))
      }
      case Proposition(_) => {
        val prop_exp = expression.asInstanceOf[Proposition]
        interp(prop_exp.nom)
      }
      case Constante(_) => {
        val const_exp = expression.asInstanceOf[Constante]
        const_exp.valeur
      }
    }

    matcher(expr)
  }

  // Question 2 :
  // renvoyer l'ensemble des noms des propositions comprises dans
  // un ensemble d'expression
  def ensembleProposition(expressions: Set[Expression]): Set[String] = {
    def matcher(expression: Expression) : Set[String] = expression match {
      case Ou(_, _) => {
        val ou_exp = expression.asInstanceOf[Ou]
        matcher(ou_exp.expGauche) ++ matcher(ou_exp.expDroite)
      }
      case Non(_) => {
        val non_exp = expression.asInstanceOf[Non]
        matcher(non_exp.exp)
      }
      case Et(_, _) => {
        val et_exp = expression.asInstanceOf[Et]
        matcher(et_exp.expDroite) ++ matcher(et_exp.expGauche)
      }
      case Implique(_, _) => {
        val imp_exp = expression.asInstanceOf[Implique]
        matcher(imp_exp.expGauche) ++ matcher(imp_exp.expDroite)
      }
      case Equivalent(_, _) => {
        val eq_exp = expression.asInstanceOf[Equivalent]
        matcher(Implique(eq_exp.expGauche, eq_exp.expDroite)) ++ matcher(Implique(eq_exp.expDroite, eq_exp.expDroite))
      }
      case Proposition(_) => {
        val prop_exp = expression.asInstanceOf[Proposition]
        Set(prop_exp.nom)
      }
      case Constante(_) => {
        Set()
      }
    }

    expressions.flatMap(matcher)
  }


  //Question 3 :
  // renvoyer la liste de toutes les interprétations possibles étant donnée
  // l'ensemble des propositions possibles dans l'ordre du code de Gray
  // exemple pour 3 propositions : FFF, FFV, FVV, FVF, VVF, VVV, VFV, VFF
  def listeInterpretation(e: Set[String]): List[Interpretation] = {

    def rec(e: Set[String]) : List[Interpretation] = {
      if (e.isEmpty)
        List()
      else if (e.size == 1)
        List(Map(e.head -> false), Map(e.head -> true))
      else {
        val original = rec(e.tail)
        val reversed = original.reverse
        original.map(Map(e.head -> false) ++ _) ++ reversed.map(Map(e.head -> true) ++ _)
      }
    }

    rec(e)
  }


  // question 4 :
  // définir si une expression est une tautologie, et si elle est consistante

  // tautologie = vrai pour tout interprétation
  def tautologie(expr: Expression): Boolean = {
    val propositions = ensembleProposition(Set(expr))
    val all_interpretations = listeInterpretation(propositions)

    if(all_interpretations.isEmpty)
      evaluation(expr)(Map())
    else {
      (for {
        interp <- listeInterpretation(propositions)
      } yield evaluation(expr)(interp)).forall(_ == true)
    }
  }

  // consistante = au moins un modèle
  def consistante(expr: Expression): Boolean = {
    val propositions = ensembleProposition(Set(expr))
    val all_interpretations = listeInterpretation(propositions)

    if(all_interpretations.isEmpty)
      evaluation(expr)(Map())
    else {
      (for {
        interp <- listeInterpretation(propositions)
      } yield evaluation(expr)(interp)).contains(true)
    }
  }


  // Question 5 :
  // définir la fonction permettant d'afficher une expression
  def affichage(expr: Expression): String = {
    def matcher(expression: Expression) : String = expression match {
      case Ou(_, _) => {
        val ou_exp = expression.asInstanceOf[Ou]
        matcher(ou_exp.expGauche) + " ou " + matcher(ou_exp.expDroite)
      }
      case Non(_) => {
        val non_exp = expression.asInstanceOf[Non]
        "!" + matcher(non_exp.exp)
      }
      case Et(_, _) => {
        val et_exp = expression.asInstanceOf[Et]
        matcher(et_exp.expGauche) + " et " + matcher(et_exp.expDroite)
      }
      case Implique(_, _) => {
        val imp_exp = expression.asInstanceOf[Implique]
        val left_exp = matcher(imp_exp.expGauche)
        val right_exp = matcher(imp_exp.expDroite)
        if(left_exp.head == '!') "!(" + left_exp.tail.mkString("") else "(" + left_exp + ") => " + (if(right_exp.head == '!') "!(" + right_exp.tail.mkString("") else "(" + right_exp) + ")"
      }
      case Equivalent(_, _) => {
        val eq_exp = expression.asInstanceOf[Equivalent]
        matcher(eq_exp.expGauche) + " <=> " + matcher(eq_exp.expDroite)
      }
      case Proposition(_) => {
        val prop_exp = expression.asInstanceOf[Proposition]
        prop_exp.nom
      }
      case Constante(_) => {
        val const_exp = expression.asInstanceOf[Constante]
        if(const_exp.valeur) "Vrai" else "Faux"
      }
    }

    matcher(expr)
  }


  // Le tableau de vérité
  class Tableau(val BC: BaseConnaissance, val Th: Theoreme) {

    override def toString: String =
      "Base de connaissances :\n" +
        BC.zipWithIndex.map { case (expr, index) => s"(${index + 1}) " + affichage(expr) }.mkString("\n") +
        "\nThéorème :\n" + affichage(Th)

    // un ligne du tableau de vérité
    class Ligne(val valeurInter: List[Boolean], val valeurBC: List[Boolean], val valeurTh: Boolean)

    // Question 6 :
    // définir les propositions ainsi que toutes les lignes du tableau de vérité
    val propositions: Set[String] = ensembleProposition(Set(Th) ++ BC.toSet).toList.sorted.toSet

    val lignes: List[Ligne] = {

      val interpretations = listeInterpretation(propositions)
      for {
        interp <- interpretations
      } yield new Ligne(interp.values.toList, BC.map(evaluation(_)(interp)), evaluation(Th)(interp))
    }

    //Question 7 :
    // Définir la fonction qui renvoie vrai si la base de connaissance infère le théorème
    def preuve: Boolean = lignes.filter((ligne) => ligne.valeurBC.forall(_ == true)).forall(_.valeurTh == true)


    //Question 8 :
    // définir une méthode qui affiche tout ou parti du tableau de vérité
    def toStringSelectif(f: Ligne => Boolean): String = {

      def convert_bool(value : Boolean) : String = {
        if(value) "V" else "F"
      }

      (
        "+---------+-------+----+" ::
        "| " + propositions.mkString(" ") + " | " + (1 to BC.size).mkString(" ") + " | Th |" ::
          "+---------+-------+----+" ::
        lignes.filter(f).map((ligne) => {
          "| " + ligne.valeurInter.flatMap(convert_bool).mkString(" ") + " | " + ligne.valeurBC.flatMap(convert_bool).mkString(" ") + " | " + convert_bool(ligne.valeurTh) + "  |"
        }).mkString("\n") ::
          "+---------+-------+----+" :: Nil).mkString("\n")
    }

  }


  // Question 9
  // à partir d'une chaine de caractère, renvoyé, si possible, l'expression correspondante :
  def parseExpression(expr: String): Option[Expression] = {

    def matcher(expression: List[String]) : Expression = expression match {
      case prop1 :: " ou " :: prop2 => {
        Ou(matcher(prop1.split("").toList), matcher(prop2))
      }
      case "!" :: reste => {
        Non(matcher(reste))
      }
      case prop1 :: " et " :: prop2 => {
        Et(matcher(prop1.split("").toList), matcher(prop2))
      }
      case prop1 :: " => " :: prop2 => {
        Implique(matcher(prop1.split(" ").toList), matcher(prop2))
      }
      case prop1 :: " <=> " :: prop2 => {
        Equivalent(matcher(prop1.split(" ").toList), matcher(prop2))
      }
      case "Vrai" :: reste => {
        Constante(true)
      }
      case "Faux" :: reste => {
        Constante(false)
      }
      case _ :: reste => {
        val is_negative = expression.head.split("!")

        if(is_negative.size == 1)
          Proposition(expression.head)
        else
          Non(Proposition(is_negative.last))
      }
    }

    println(expr.split(" ").toList)

    Some(matcher(expr.split(" ").toList))
  }

}







