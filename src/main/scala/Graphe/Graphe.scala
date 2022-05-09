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

    def rec_distance(current: Noeud, old_neighbors : Set[Noeud], distance : Int): Option[Int] = {
        if(current == arrive)
          Option(distance)
        else{

          val neighbors = voisins(current) -- old_neighbors
          if(neighbors.isEmpty)
            None
          else {
            val res = for {
              voisin <- voisins(current) -- old_neighbors
              if(rec_distance(voisin, old_neighbors ++ Set(voisin), distance + 1).isDefined)
            } yield rec_distance(voisin, old_neighbors ++ Set(voisin), distance + 1)

            if(res.isEmpty)
              None
            else {
              res.min
            }

          }
        }

    }

    rec_distance(depart,Set(), 0)

  }

  lazy val composantesConnexes: Set[Set[Noeud]] = ???

  lazy val estBicoloriable: Boolean = ???

}
