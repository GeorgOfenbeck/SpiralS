import ch.ethz.spirals.dsls.DFT

/**
 * Georg Ofenbeck
 First created:
 * Date: 13/06/13
 * Time: 13:09 
 */

val exhaust = for (i <- 2 until 6)
  yield ch.ethz.spirals.search.Search.ExhaustiveSearch(DFT(math.pow(2,i).toInt,1))
println("----------------------------")
println("Now doing DP")
val x = for (i <- 2 until 6)
 yield ch.ethz.spirals.search.Search.DP(DFT(math.pow(2,i).toInt,1))


ch.ethz.spirals.search.Search.scoreMap map( x=> println(x._1.size , x._2._2))


def infix_tensor (x: Rep[SPL], y: Rep[SPL]): Rep[SPL]
def infix_compose(x: Rep[SPL], y: Rep[SPL]): Rep[SPL]

val DFT_CT: Rule = {
  case DFT(n) if n > 2 && !isPrimeInt(n) =>
    val (m,k) = choose_factorize(n)
    (DFT(k) tensor I(m)) compose T(n,m)
    compose (I(k) tensor DFT(m)) compose L(n,k)
}

override def transformStm(stm: Stm):Exp[Any]= stm.rhs match {
  case Compose(a, Const(I(n))) => a
  case Compose(Const(I(n)), b) => b
}