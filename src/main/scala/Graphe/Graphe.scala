package Graphe

import scala.annotation.tailrec

case class Noeud(nom: String)

case class Arc(extremite1: String, extremite2: String)

case class Graphe(noeuds: Set[Noeud], arcs: Set[Arc]) {

  def +(arc: Arc): Graphe = ???

  def +(autre: Graphe): Graphe = ???

  def voisins(noeud: Noeud): Set[Noeud] = ???

  def degre(noeud: Noeud): Int = ???

  def distance(depart: Noeud, arrive: Noeud): Option[Int] = ???

  lazy val composantesConnexes: Set[Set[Noeud]] = ???

  lazy val estBicoloriable: Boolean = ???

}
