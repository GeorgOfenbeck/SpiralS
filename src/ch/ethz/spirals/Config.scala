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

package ch.ethz.spirals.conf

case class SpiralSConfig( var id : Int,
                          var debug : Boolean,
                          var verbosity : Int,
                          var random : Boolean,
                          var seed : Int,
                          var develop: Boolean,
                          var comment: String)

case class SearchConfig(id : Int,
                        unrollSize: Int)
{
  def print () = println("SearchConfig:\n - unrollSize = " + unrollSize)
}

case class CodeGenConfig(id : Int,
                         generateComments: Boolean,
                         useCodeFormatter: Boolean,
                         comment : String)

case class ValidationConfig (id: Int,
                             useFFTW: Boolean, //the other option being JTransform
                             threshold: Double, //allowed error
                             verificationCycles: Int, //nr of repeats per tested code
                             compareMatricesThreshold : Int, //size for which to fall back to comparing matrices (costly!)
                             comment : String)

case class Configuration ( var spiralsconfig : SpiralSConfig,
                    searchconfig: SearchConfig,
                    codegenconfig: CodeGenConfig,
                    validationconfig: ValidationConfig
                    )


object Config
{

  def apply() : Configuration = ch.ethz.spirals.db.DB.getConf()

  def print() = {
    val t = this()
    t.searchconfig.print()
  }


}
