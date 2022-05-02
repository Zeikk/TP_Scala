import ASCIIart._

art1

art2

val ascii = new ASCIIart(art1)

ascii.hauteur
ascii.largeur

ascii.liste.grouped(1).toList

val split = ascii.liste.map((elem) => elem.grouped(ascii.largeur).toList).transpose

val alphabet : List[Char] = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
val map = (alphabet zip split).toMap

map.apply('s').mkString("\n")