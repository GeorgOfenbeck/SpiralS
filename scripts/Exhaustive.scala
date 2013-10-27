/**
 * Georg Ofenbeck
 First created:
 * Date: 29/07/13
 * Time: 13:39 
 */

import ch.ethz.spirals.dsls.DFT



val exhaust = for (i <- 3 until 24)
yield ch.ethz.spirals.search.Search.ExhaustiveSearch(DFT(i,1))
println("----------------------------")
