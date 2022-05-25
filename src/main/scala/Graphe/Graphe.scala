package Graphe

case class Noeud(nom: String)

case class Arc(extremite1: String, extremite2: String)

case class Graphe(noeuds: Set[Noeud], arcs: Set[Arc]) {

  def +(arc: Arc): Graphe = Graphe(noeuds = noeuds + Noeud(arc.extremite1) + Noeud(arc.extremite2), arcs = arcs + arc)

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
        } yield(matcher(noeud, old_neighbors + noeud,  Some(distance.get + 1)))

        if(res.isEmpty || res.forall(_.isEmpty)) None  // vérification si une solution existe
        else res.filter(_.isDefined).min
      }
    }

    matcher(depart, Set(), Some(0))
  }

  def deep_search(current : Noeud, old_neightbors : Set[Noeud]) : Set[Noeud] = {
    val neightbors = voisins(current) -- old_neightbors
    if(neightbors.isEmpty)
      old_neightbors + current
    else {
      neightbors.flatMap(deep_search(_, old_neightbors + current))
    }

  }

  lazy val composantesConnexes: Set[Set[Noeud]] = for {
      node <- noeuds
  } yield deep_search(node, Set())


  lazy val estBicoloriable: Boolean = {
    def check(firstColor: Set[Noeud], secondColor: Set[Noeud], composant : Set[Noeud]): Boolean = {
      if (firstColor.intersect(secondColor).nonEmpty)
        false
      else
        if(composant.equals(firstColor ++ secondColor))
          true
        else
          check(firstColor ++ secondColor.flatMap(voisins), secondColor ++ firstColor.flatMap(voisins), composant)
    }

    (for {
      composant <- composantesConnexes
    } yield check(Set(composant.head), Set(), composant)).forall(_ == true)
  }

}
