/**
 *  SpiralS - ETH Zurich
 *  Copyright (C) 2013  Georg Ofenbeck (ofenbeck@inf.ethz.ch)
 *
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see http://www.gnu.org/licenses/.
 */

import org.scalatest.FunSpec


class GPCE2013 extends FunSpec {

  //this should lunch a search to create results such as shown in the GPCE2013 paper

  //note that the results will not be optimal cause the default timing mechanism is a simple time measurement done
  //via BridgeJ

  //for the results in the paper we used cycle accurate timings through perfplot (which utilizes Intel PCM).
  //As this requires you to be admin on your machine and is a security risk we disabled it by default.

  //Also make sure that you have either gcc or icc in your path!

  describe("Test"){
    ch.ethz.spirals.db.DB.checkCreate() //check if DB exits - otherwise create
    import ch.ethz.spirals.search.Search._
    import ch.ethz.spirals.dsls._
    import ch.ethz.spirals.db.Queries._


    val upper_limit = 10 //increasing is fine - just be aware that it takes a long time due to the gigantic code size

    for (i <- 2 until upper_limit)
      DP(DFT(Math.pow(2,i).toInt,1))

    for (i <- 2 until upper_limit){
      val dft = DFT(Math.pow(2,i).toInt,1)
      println("results for " + dft.toString)
      get_Scores(dft,1)
    }

  }
}
