// les packages object permettent de définir des méthodes/variables disponibles dans tout le package
package object EnsemblesFonctions {

  // type est l'équivalent de typedef en C
  // On représente des ensembles à l'aide de fonctions Int => Boolean
  type Ensemble = Int => Boolean

  // indique si un ensemble contient un élément
  def contient(s: Ensemble, elem: Int): Boolean = s(elem)

  // limite est utilisé pour l'affichage, ainsi que les fonctions pourTout et ilExiste
  val limite = 100

  // Renvoie un ensemble sous forme de chaîne de caractères
  // compris entre -limite et +limite
  def chaine(s: Ensemble): String = {
    val xs = for (i <- -limite to limite if contient(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  // affiche un ensemble
  def afficheEnsemble(s: Ensemble): Unit = println(chaine(s))


  /** *********************************************/
  /*        méthodes à définir ci-dessous        */
  /** *********************************************/

  // définir une méthode qui renvoie un singleton : l'ensemble qui ne contient que elem
  // i.e. la fonction qui renvoie vrai si on lui passe elem en paramètre, et faux sinon
  def singleton(elem: Int): Ensemble = (x : Int) => x == elem

  // définir une méthode qui renvoie l'union de deux ensembles
  def union(s: Ensemble, t: Ensemble): Ensemble = (x : Int) => s(x) || t(x)

  // définir une méthode renvoyant l'intersection de deux ensembles
  def intersection(s: Ensemble, t: Ensemble): Ensemble = (x : Int) => s(x) && t(x)

  // définir une méthode diff qui renvoie la différence de deux ensembles (dans s, pas dans t)
  def difference(s: Ensemble, t: Ensemble): Ensemble = (x : Int) => s(x) && !t(x)

  // définir une méhtode complement qui revoie le compléement d'un ensemble
  def complement(s: Ensemble): Ensemble = (x : Int) => !s(x)

  //définir une méthode filtre, qui renvoie le sous ensemble pour lequel p est vraie
  def filtrer(s: Ensemble, p: Int => Boolean): Ensemble = (x : Int) => s(x) && p(x)

  // définir une méthode pourTout, qui vérifie si p est vrai pour tout élément de s (de -limite à +limite)
  def pourTout(s: Ensemble, p: Int => Boolean): Boolean = {

    def pourToutRec(s: Ensemble, p: Int => Boolean, x: Int): Boolean = {

      if (x > limite) true
      else if(s(x))
        p(x) && pourToutRec(s, p, x+1)
      else pourToutRec(s, p, x+1)
    }

    pourToutRec(s, p, -limite)
  }

  // définir une méthode ilExiste qui renvoie vrai si un élément renvoie vraie pour p
  def ilExiste(s: Ensemble, p: Int => Boolean): Boolean = {

    def ilExisteRec(s: Ensemble, p: Int => Boolean, x: Int): Boolean = {
      if (x > limite) false
      else if(s(x))
        p(x) || ilExisteRec(s, p, x+1)
      else ilExisteRec(s, p, x+1)
    }

    ilExisteRec(s, p, -limite)
  }

  // définir une fonction image qui renvoie l'ensemble image de s
  def image(s: Ensemble, f: Int => Int): Ensemble = (y: Int) => ilExiste(s, (x: Int) => f(x) == y)

}
