package ASCIIart

class ASCIIart(art: String) {

  val (largeur, hauteur, liste) = {
    val f = art.split('\n')
    (f(0).toInt, f(1).toInt, f.drop(2).toList)
  }

  lazy val tableLettre: Map[Char, List[String]] = listeToMap()

  // question 1 : renvoie la Map qui associe chaque lettre à son ascii art
  // attention : si la lettre n'est pas définie, il fait renvoyer l'ascii-art du symbole ? fourni en derniere position
  private def listeToMap(): Map[Char, List[String]] = {

    val alphabet : List[Char] = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '?')

    val letters = liste.map((elem) => elem.grouped(largeur).toList).transpose
    (alphabet zip letters).toMap.withDefaultValue(letters.last)
  }

  // question 2 : renvoie le mot sous forme d'ascii art
  def apply(mot: String): String = {
    mot.map( tableLettre ).transpose.map( _.mkString("") ).mkString("\n")
  }
}
