package object Parentheses {

  // Écrire une fonction récursive qui indique si une phrase dispose de parenthèses bien construite
  def equilibre(phrase: String): Boolean = {
    def matcher(list: List[Char], score: Int) : Boolean = list match {
      case Nil => score == 0
      case '(' :: reste => matcher(reste, score + 1)
      case ')' :: reste => if(score <= 0) false else matcher(reste, score -1)
      case _ => matcher(list.tail, score)
    }

    matcher(phrase.toList, 0)
  }

  // pareil, mais générique
  def equilibreGenerique(co: Char, cf: Char)(phrase: String): Boolean = {

    def matcher(list: List[Char], score: Int) : Boolean = list match {
      case Nil => score == 0
      case `co` :: reste => matcher(reste, score + 1)
      case `cf` :: reste => if(score <= 0) false else matcher(reste, score -1)
      case _ => matcher(list.tail, score)
    }

    matcher(phrase.toList, 0)
  }

  // utiliser la fonction générique pour définir la version xml avec < et > comme caractère ouvrant/fermant
  lazy val equilibreXml: String => Boolean = equilibreGenerique('<', '>')

}
