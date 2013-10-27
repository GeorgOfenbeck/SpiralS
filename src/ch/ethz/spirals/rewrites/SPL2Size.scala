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


package ch.ethz.spirals.rewrites


import scala.virtualization.lms._
import internal._

/**
 * Georg Ofenbeck
 First created:
 * Date: 27/05/13
 * Time: 21:16 
 */
trait SPL2Size extends GraphTraversal{
  import ch.ethz.spirals.dsls._
  val IR : SPL_Exp
  import IR._

  import scala.collection.mutable.HashMap
  //this returns the size (in terms of vector size) of an SPL expression
  def emitSize(start: Exp[Any]): Int = {

    val deflist = buildScheduleForResult(start)
    val index_array = new HashMap[Int,Int]
    val size_array = new Array[Int](deflist.size)

    var i : Int = 0
    for (TP(sym, rhs) <- deflist) {
      val index = sym match {
        case Sym(n) => n
        case _ => -1
      }
      index_array += (index -> i)
      size_array(i) = emitSize_Node(sym,rhs,size_array,index, index_array)//my_emitNode(sym, rhs,f_array, matrices, index )
      i = i + 1
    }
    size_array.last
  }


  def emitSize_Node(sym: Sym[Any], rhs: Def[Any], size_array: Array[Int], symnr: Int, lt : HashMap[Int,Int])  = rhs match
  {
    //--------------------------------Compose -----------------------------
    case DirectSum(a1,b1) =>
    {
      (a1,b1) match {
        case (Sym(a),Sym(b)) => size_array(lt(a)) + size_array(lt(a))
        case (Sym(a),Const(b: SPL))=> size_array(lt(a)) + b.size
        case (Const(b: SPL),Sym(a))=> size_array(lt(a)) + b.size
        case (Const(a: SPL),Const(b: SPL))=> a.size + b.size
      }
    }


    //--------------------------------Compose -----------------------------
    case Compose(a1,b1) =>
    {
      (a1,b1) match {
        case (Sym(a),Sym(b)) => size_array(lt(a))
        case (Sym(a),Const(b: SPL))=> b.size
        case (Const(b: SPL),Sym(a))=> b.size
        case (Const(a: SPL),Const(b: SPL))=> a.size
      }
    }
    //-------------------------------Tensor--------------------------------
    case Tensor(a1,b1) =>
    {
      (a1,b1) match {
        case (Const(I(n)),Sym(b)) => size_array(lt(b)) * n
        case (Const(I(n)),Const(b: SPL)) => b.size * n
        case (Sym(a),Const(I(n))) => size_array(lt(a)) * n
        case (Const(a: SPL),Const(I(n))) => a.size * n
      }
    }
  }




}
