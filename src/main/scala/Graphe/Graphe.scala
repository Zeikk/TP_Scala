package Graphe

import scala.annotation.tailrec

case class Noeud(nom: String)

case class Arc(extremite1: String, extremite2: String)

case class Graphe(noeuds: Set[Noeud], arcs: Set[Arc]) {

  def +(arc: Arc): Graphe = Graphe(noeuds = noeuds ++ Set(Noeud(arc.extremite1), Noeud(arc.extremite2)), arcs = arcs ++ Set(arc))

  def +(autre: Graphe): Graphe = Graphe(noeuds = noeuds ++ autre.noeuds, arcs = arcs ++ autre.arcs)

  def voisins(noeud: Noeud): Set[Noeud] = {

    arcs.filter((elem) => elem.extremite1 == noeud.nom || elem.extremite2 == noeud.nom).map((elem) => {
      if(elem.extremite1 == noeud.nom)
        Noeud(elem.extremite2)
      else
        Noeud(elem.extremite1)
    })
  }

  def degre(noeud: Noeud): Int = voisins(noeud).size

  def distance(depart: Noeud, arrive: Noeud): Option[Int] = {

    def matcher(current: Noeud, old_neighbors : Set[Noeud], distance : Option[Int]) : Option[Int] = current match {
      case `arrive`=> distance
      case _ => {
        val res = for {
          noeud <- voisins(current) -- old_neighbors
        } yield(matcher(noeud, old_neighbors ++ Set(noeud),  Some(distance.get + 1)))

        if(res.isEmpty) None else {

          if(res.filter(_.isDefined).isEmpty)
            None
          else
            res.filter(_.isDefined).min
        }
      }
    }

    matcher(depart, Set(), Some(0))
  }

  def deep_search(current : Noeud, old_neightbors : Set[Noeud]) : Set[Noeud] = {
    val neightbors = voisins(current) -- old_neightbors
    if(neightbors.isEmpty)
      old_neightbors + current
    else {
      neightbors.flatMap((node) => deep_search(node, old_neightbors + current))
    }

  }

  lazy val composantesConnexes: Set[Set[Noeud]] = for {
      node <- noeuds
    } yield (deep_search(node, Set()))


  lazy val estBicoloriable: Boolean = composantesConnexes.count(_.size > 1) == 2

}
