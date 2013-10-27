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

import virtualization.lms._
import common._
import internal._

import scala.math._

import ch.ethz.spirals.dsls._


import ch.ethz.spirals.conf._
import ch.ethz.spirals.util._
//-------------------------------------------------------------------------------
//Data Structures used to store the nodes of the RuleTree

//after 1st application:
// BreakDown ( DFT(x),
//               rule: DFT_CT
//               rule2spl: f
//               children: DFT(x1), DFT(x2)
//           )
//               where DFT(x1) and DFT(x2) look like
//
//            BreakDown( DFT(x1), None)

//The idea is that the Option Type is replace during the search (copy)

//The final node should like like this:
// BreakDown (DFT(2),
//              rule: DFT_Base
//              rule2spl: f (just take child)
//              children: F2

//-----------------------------------------------------------------------------
//this is used once its decided which breakdown to use (rule/children)
class BreakDown_resolved (
                           val rule : PartialFunction[SPL,List[BreakDown]],
                           val children : List[BreakDown]
                           )
//this stores the non terminal and possibly the rule that is applied

class BreakDown(
                 val nt: SPL,
                 val applied_bd: Option[BreakDown_resolved]
                 )
{
  def this(nt: SPL) = this(nt,None)

  override def toString =
  {



    //=================================================================
    def PrintRuleTree(in_bd : BreakDown): String = {
      var outstring = ""
      val rule_names = List("WHT_Base", "WHT_Base", "DFT_CT", "DFT_Base", "DFT_Rader", "DFT_GoodThomas", "WHT_Base")

      printrecurse(in_bd,0)

      def printrecurse(in_bd: BreakDown, level: Int)
      {
        for (i <- 0 until level)
          outstring = outstring + " "
        outstring = outstring + in_bd.nt.toString +"\n"

        in_bd.applied_bd map (bd_applied =>
          {
            for (i <- 0 until level)
              outstring = outstring + " "
            outstring = outstring + rule_names(BreakdownRules.all.indexOf(bd_applied.rule)) + "\n"

          bd_applied.children map ( child => printrecurse(child,level+1))
          }
          )

      }
      outstring
    }
    PrintRuleTree(this)
  }

}
//=================================================================================

trait BreakDown2Latex{
  def bd2latex(in_bd: BreakDown): String = {
    "\\begin{tikzpicture}[grow=right, sloped]\n    \\" + bd2latex_recurse(in_bd) + ";\n    \\end{tikzpicture}"
  }

  def bd2latex_recurse(in_bd: BreakDown): String =
  {
    var latstring = "node[bag] {$"+in_bd.nt.toLatex() + "$}\n"
    in_bd.applied_bd map ( x =>
    {
      x.rule match
      {
        case BreakdownRules.DFT_CT => {
          //thats not nice
          val k = in_bd.nt match {
            case DFT(n, k) => k
            case _ => 1
          }

          val k1 = x.children(0).nt.size //divisors(0)
          val d1 = x.children(1).nt.size //divisors(1)
          val n1 = k1 * d1
          //variable : Type                   = Parameter => body
          x.children map (child =>
          {
            latstring += "child {\n"
            latstring += bd2latex_recurse(child)
            latstring += "edge from parent\n"
            latstring += "node[above] {CT}\n"
            latstring += "node[below] {"+k1+","+d1+"}\n"
            latstring += "}\n"
          })
        }
        case BreakdownRules.DFT_Base => {
          latstring += "child {\nnode[end, label=right:\n{F2}] {}\n                edge from parent\n                node[above] {Base}\n}\n"
        }

        case BreakdownRules.DFT_SplitRadix => {
          x.children map (child =>
          {
            latstring += "child {\n"
            latstring += bd2latex_recurse(child)
            latstring += "edge from parent\n"
            latstring += "node[above] {SplitRadix}\n"
            latstring += "\n}"
          })

        }
        case BreakdownRules.DFT_GoodThomas => {
          val m1 = x.children(0).nt.size //divisors(0)
          val n1 = x.children(1).nt.size //divisors(1)
          val mn = m1 * n1
          x.children map (child =>
          {
            latstring += "child {\n"
            latstring += bd2latex_recurse(child)
            latstring += "edge from parent\n"
            latstring += "node[above] {PrimeFactor}\n"
            latstring += "node[below] {"+m1+","+n1+"}\n"
            latstring += "}\n"
          })
          latstring += "edge from parent\n"

        }

        case BreakdownRules.DFT_Rader => {
          x.children map (child =>
          {
            latstring += "child {\n"
            latstring += bd2latex_recurse(child)
            latstring += "edge from parent\n"
            latstring += "node[above] {Rader}\n"
            latstring += "}\n"
          })

        }

        case _ => assert(false)

      }

    }
      )
    latstring
  }

}






trait BreakDown2SPL{
  val IR: SPL_Exp
  import IR._


  def bd2spl(in_bd: BreakDown): Rep[SPL] = {
    val r: Rep[SPL] = in_bd.applied_bd map (x =>
      x.rule match {
        case BreakdownRules.DFT_CT => {
          //thats not nice
          val k = in_bd.nt match {
            case DFT(n, k) => k
            case _ => 1
          }

          val k1 = x.children(0).nt.size //divisors(0)
          val d1 = x.children(1).nt.size //divisors(1)
          val n1 = k1 * d1
          //variable : Type                   = Parameter => body
          val extended_children = x.children map (child => bd2spl(child))
          val c1 = extended_children(0)
          val c2 = extended_children(1)

          val spl_expression = {
            //SPL - this is the actual SPL expression - c1 and c2 are the possibly extended children
            (c1 tensor I(d1)) compose T(n1, d1, k) compose (I(k1) tensor c2) compose L(n1, k1)
            //--------------------------------------------------------------------------------
          }
          spl_expression
        }
        case BreakdownRules.DFT_Base => {
          val spl: Rep[SPL] = F_2()
          spl
        }

        case BreakdownRules.DFT_SplitRadix => {
          //thats not nice
          val (n, k) = in_bd.nt match {
            case DFT(n, k) => (n, k)
            case _ => (-1, -1)
          }
          val extended_children = x.children map (child => bd2spl(child))
          val c1 = extended_children(0)
          val c2 = extended_children(1)

          //this is the actual SPL expression - c1 and c2 are the possibly extended children
          val spl_expression =
            (F_2() tensor I(n / 2)) compose (c1 directsum (
              (
                //((F_2() compose F_2()) tensor I(n / 4))
                ((D2(k) compose F_2()) tensor I(n / 4))
                  compose T3L(n / 2, 2, k)
                //(T3(n/2,2,k) compose L(n/2,2)) //Used T3L here cause its a permute on the Diagonal - not on a matrix
                ) //in original Spiral there is function fCompose for this
                compose (I(2) tensor c2) compose L(n / 2, 2)
              )
              ) compose L(n, 2)
          spl_expression
        }
        case BreakdownRules.DFT_GoodThomas => {

          val extended_children = x.children map (child => bd2spl(child))
          val c1 = extended_children(0)
          val c2 = extended_children(1)

          val m1 = x.children(0).nt.size //divisors(0)
          val n1 = x.children(1).nt.size //divisors(1)
          val mn = m1 * n1

          //this is the actual SPL expression - c1 and c2 are the possibly extended children
          val spl_expression =
            Vt(m1, n1, 1, 1) compose ((c1 tensor I(n1)) compose (I(m1) tensor c2)) compose V(m1, n1, 1, 1)
          spl_expression
        }

        case BreakdownRules.DFT_Rader => {
          //thats not nice
          val (n, k) = in_bd.nt match {
            case DFT(n, k) => (n, k)
            case _ => (-1, -1)
          }
          val root = MathUtilities.PrimitiveRoot(n)
          //println("ROOT: ", root)
          val extended_children = x.children map (child => bd2spl(child))
            val c1 = extended_children(0)
            val c2 = extended_children(1)
            //this is the actual SPL expression - c1 and c2 are the possibly extended children
            val spl_expression = (Wt(n,1,root) compose (I(1) directsum c1) compose  (Rader_Mid_Matrix(n) directsum Rader_Diag(n,k,root)) compose  (I(1) directsum c2) compose W(n,1,root))
            spl_expression
        }



        case _ =>
          println(x)
          assert(false);
          val spl: Rep[SPL] = F_2()
          spl
      }
      ) getOrElse (
      {
        // assert(false);
        val spl: Rep[SPL] = F_2()
        spl
      }
      )
    r

  }

  /*
  def extend_children (children: List[BreakDown]) : List[Rep[rSPL]] =
  {
    val extended_children : List[Rep[rSPL]] = children map (child => child.applied_bd map
      (
        child_bd_exists => child_bd_exists.rule2spl(child_bd_exists.children)
        ) getOrElse( (F_2() tensor F_2()) )  //2DO - do some proper error handling here!
      )
    //The idea here is that the final case will not recurse further (so DFT_Base.rule2spl will not call its childrens rule2spl)
    extended_children
  }*/

}


object BreakdownRules {
  import ch.ethz.spirals.util._


  def correct_k(in_bd: BreakDown, kSPL: SPL): BreakDown =
  {

    def recurse (in_bd: BreakDown, kSPL: SPL): BreakDown =
    {
      println(in_bd.nt)
      val parent_nt = in_bd.nt
      val children_bd = in_bd.applied_bd.get.children
      val children_nt = children_bd map (x => x.nt)
      val rule = in_bd.applied_bd.get.rule
      val possible_breakdowns = rule.apply(parent_nt)

      var ret = -1

      for (bd <- possible_breakdowns)
      {
        val cnt = bd.applied_bd.get.children map (x => x.nt)
        if (cnt == children_nt)
          ret = possible_breakdowns.indexOf(bd)
      }
      def getnk(in: SPL): (Int,Int) =
      {
        in match {
          case DFT(n,k) => (n,k)
          case _ => (0,0)
        }
      }

      val (nnew,knew) = getnk(kSPL)
      val (n,k) = getnk(parent_nt)

      val new_bd = if (n > 2)
      {
        val possible_breakdowns2 = rule.apply(DFT(n,knew))
        val concrete_bd = possible_breakdowns2(ret)
        val expanded_children = concrete_bd.applied_bd map ( bd_existing => bd_existing.children map (child =>
        {

          recurse(
            in_bd.applied_bd.get.children(bd_existing.children.indexOf(child)),
            child.nt
          )
        }))
        new BreakDown(DFT(n,knew),
          Some(
            new BreakDown_resolved(in_bd.applied_bd.get.rule,expanded_children.get)
          )
        )
      }
      else
      {
        in_bd //assuming that we are at a F2 at this point
      }
      new_bd
    }

    recurse(in_bd, kSPL)
  }

  def GetBDDFIndex (in_bd : BreakDown) : Int =
  {
    val parent_nt = in_bd.nt
    val children_bd = in_bd.applied_bd.get.children
    val children_nt = children_bd map (x => x.nt)
    val rule = in_bd.applied_bd.get.rule
    val rule_index = ch.ethz.spirals.rewrites.BreakdownRules.all.indexOf(rule)

    val possible_breakdowns = rule.apply(parent_nt)

    var ret = -1

    for (bd <- possible_breakdowns)
    {
      val cnt = bd.applied_bd.get.children map (x => x.nt)
      if (cnt == children_nt)
        ret = possible_breakdowns.indexOf(bd)
    }
    ret
  }



// DFT_CT: 1965
//   General Cooley-Tukey Rule
//   DFT_n = (DFT_n/d tensor I_d) * diag * (I_n/d tensor F_d) * perm
//
// Cooley/Tukey:
//   An Algorithm for the Machine Calculation of Complex Fourier Series.
//   Mathematics of Computation, Vol. 19, 1965, pp. 297--301.

  /*
  val WHT_CT : PartialFunction[SPL,List[BreakDown]] =
  {
    case (WHT(n))
      if (n > 2 ) => //this is the guard
    {
      if ( Config().spiralsconfig.verbosity > 0 ) println("CT")
      def get_rule(divisors : List[Int]): (List[BreakDown] => Rep[rSPL]) =
      {
        val k1 = divisors(0)
        val d1 = divisors(1)
        val n1 = k1*d1

         //variable : Type                   = Parameter => body
         val rule : (List[BreakDown] => Rep[rSPL]) = (children: List[BreakDown]) =>
        {
          val extended_children = extend_children(children)
          val c1 = extended_children(0)
          val c2 = extended_children(1)

          val spl_expression =
          //SPL - this is the actual SPL expression - c1 and c2 are the possibly extended children
            ( c1 tensor I(d1) ) compose ( I(k1) tensor c2)
          //--------------------------------------------------------------------------------
          spl_expression
        }
        rule
      }
      //create Breakdown option with uninitialized children (no breakdown defined yet - only Non Terminal
      MathUtilities.DivisorPairs(n).map(pair=> new BreakDown(WHT(n),
                                     Some( new BreakDown_resolved(
                                              WHT_CT,
                                              get_rule(pair),
                                             List(
                                               new BreakDown(WHT(pair(0))),
                                               new BreakDown(WHT(pair(1)))
                                             )))))
    }
  }
  */


  // DFT_CT: 1965
  //   General Cooley-Tukey Rule
  //   DFT_n = (DFT_n/d tensor I_d) * diag * (I_n/d tensor F_d) * perm
  //
  // Cooley/Tukey:
  //   An Algorithm for the Machine Calculation of Complex Fourier Series.
  //   Mathematics of Computation, Vol. 19, 1965, pp. 297--301.


  val DFT_CT : PartialFunction[SPL,List[BreakDown]] =
  {
    case (DFT(n,k))
      if (n > 2 && !MathUtilities.IsPrimeInt(n)) => //this is the guard
    {
      if ( Config().spiralsconfig.verbosity > 0 ) println("CT")

      //create Breakdown option with uninitialized children (no breakdown defined yet - only Non Terminal
      MathUtilities.DivisorPairs(n).map(pair=> new BreakDown(DFT(n,k),
                                     Some( new BreakDown_resolved(
                                              DFT_CT,
                                              //get_rule(pair),
                                             List(
                                               new BreakDown(DFT(pair(0),MathUtilities.mathmod(k,pair(0)))),
                                               new BreakDown(DFT(pair(1),MathUtilities.mathmod(k,pair(1))))
                                             )))))
    }
  }

  //The final node should like like this:
  // BreakDown (DFT(2),
  //              rule: DFT_Base
  //              rule2spl: f (just take child)
  //              children: F2
  val DFT_Base : PartialFunction[SPL,List[BreakDown]] =
  {
    case (DFT(n,k))
      if (n == 2) => //this is the guard
    {



      List(new BreakDown(DFT(n,k), Some( new BreakDown_resolved(
                              DFT_Base,
                              //f,
                              List(new BreakDown(F_2()))
                                ))
      ))
    }
  }


  //TODO: merge with DFT_Base
  //The final node should like like this:
  // BreakDown (DFT(2),
  //              rule: DFT_Base
  //              rule2spl: f (just take child)
  //              children: F2

  val WHT_Base : PartialFunction[SPL,List[BreakDown]] =
  {
    case (WHT(n))
      if (n == 2) => //this is the guard
    {
      List(new BreakDown(WHT(n), Some( new BreakDown_resolved(
        WHT_Base,List()
      ))
      ))
    }
  }

  /*
  #F DFT_SplitRadix: 1984
  #F
  #F DFT_n = B * (DFT_n/2 dirsum DFT_n/4 dirsum DFT_n/4) * perm
  #F
  #F B = (DFT_2 tensor I_n/2) * S * diag
  #F S = (I_n/2 dirsum (diag([1,E(4)] tensor I_n/4))
  #F
  #F Duhamel, Pierre:
  #F   Split Radix FFT Algorithm, Electronics Letters, Vol. 20, No. 1,
  #F   pp. 14--16, 1984
  #F
  */

  val DFT_SplitRadix : PartialFunction[SPL,List[BreakDown]] =
  {
    case (DFT(n,k))

      if (n >= 8 && (n % 4 == 0) && n <9 ) => //this is the guard
    {
      if ( Config().spiralsconfig.verbosity > 0 ) println("SplitRadix")


      //create Breakdown option with uninitialized children (no breakdown defined yet - only Non Terminal
      List(new BreakDown(DFT(n,k),
        Some( new BreakDown_resolved(
          DFT_SplitRadix,
          List(
            new BreakDown(DFT(n/2,k )),
            new BreakDown(DFT(n/4,k )))
        ))
      )
      )
    }


  }

//  #F DFT_Rader: Rader's Algorithm for Prime size DFT
//  #F
//  #F   DFT(p) -> P(g)' * (1 dirsum DFT(p-1)) * Tp * (1 dirsum DFT(p-1)) * P(g)",
//  #F   P(g) = perm
//  #F   Tp   = [[1,1],[1,-1/(p-1)]] dirsum diag
//  #F

  val DFT_Rader : PartialFunction[SPL,List[BreakDown]] =
  {
    case (DFT(n,k))
      if (n > 2 && MathUtilities.IsPrimeInt(n) ) => // && k < 2 && k > -2) =>       //the limitation on k is cause i suspect a bug in spiral
    {
      if ( Config().spiralsconfig.verbosity > 0 ) println("Rader")





      List(new BreakDown(DFT(n,k), Some( new BreakDown_resolved(
                                  DFT_Rader,
                                  List( new BreakDown(DFT(n-1,-1)),
                                        new BreakDown(DFT(n-1,-1))
                                  )
      ))))
    }
  }


//  #F DFT_GoodThomas : Prime Factor FFT
//  #F
//  #F     DFT_n*k -> perm * (DFT_n_a tensor DFT_k_b) * perm
//  #F     when gcd(n,k) = 1
//  #F

  val DFT_GoodThomas : PartialFunction[SPL,List[BreakDown]] =
  {
    case (DFT(n,k))
      if (n > 2  && !MathUtilities.DivisorPairsRP(n).isEmpty ) =>
      // && n <= Config.unroll_size) => //this is the guard
    {
      if ( Config().spiralsconfig.verbosity > 0 ) println("GoodThomas")

      //create Breakdown option with uninitialized children (no breakdown defined yet - only Non Terminal
      MathUtilities.DivisorPairsRP(n).map(pair=> new BreakDown(DFT(n,k),
        Some( new BreakDown_resolved(
          DFT_GoodThomas,
          List(
            new BreakDown(DFT(pair(0), MathUtilities.mathmod(k*pair(1),pair(0) ))),
            new BreakDown(DFT(pair(1), MathUtilities.mathmod(k*pair(0),pair(1) )))
          )))))
    }


  }



  /*
  //This rule seems to be buggy - not enabled for the moment since not needed
  val DFT_Canonize : PartialFunction[SPL,List[BreakDown]] =
  {
    case (DFT(n,k))
      if (n > 2 && abs(k) != 1) =>
    {
      if ( Config().spiralsconfig.verbosity > 0 ) println("DFT_Canonize for " + k)
      val root = MathUtilities.PrimitiveRoot(n)


      val f :  (List[BreakDown] => Rep[rSPL]) = ( children: List[BreakDown] ) =>
      {
        val extended_children = extend_children(children)
        val c1 = extended_children(0)
        //this is the actual SPL expression
        val spl_expression =
          OddStride(n,k) compose c1
        //------
        spl_expression
      }

      List(new BreakDown(DFT(n,k), Some( new BreakDown_resolved(
        DFT_Canonize,
        f,
        List( new BreakDown(DFT(n,1))
        )
      ))))
    }
  }   */

  //--------------------------------------------------------------------------
  // This controls which rules will be used during the Search
  //val all = List(DFT_Rader,DFT_Base, DFT_CT, DFT_GoodThomas ,  DFT_SplitRadix )



  //This is the list of all the rules
  //DONT CHANGE THE ORDER!!!!
  //will break the config within the database (as it uses the positions)
  //val all = List(WHT_CT, WHT_Base, DFT_CT, DFT_Base, DFT_Rader, DFT_GoodThomas,  DFT_SplitRadix)


  //val all = List(WHT_Base, WHT_Base, DFT_CT, DFT_Base, DFT_Rader, DFT_GoodThomas,  DFT_SplitRadix)
  //val all = List(WHT_Base, WHT_Base, DFT_CT, DFT_Base, DFT_Rader, DFT_GoodThomas,  WHT_Base)
  //val all = List(WHT_Base, WHT_Base, DFT_CT, DFT_Base, DFT_Rader, DFT_GoodThomas,  WHT_Base)
  val all = List(WHT_Base, WHT_Base, DFT_CT, DFT_Base, DFT_Rader, WHT_Base,  WHT_Base)
  //val all = List(WHT_Base, WHT_Base, DFT_CT, DFT_Base, DFT_Rader, DFT_GoodThomas,  WHT_Base)
  //val all = List(WHT_Base, WHT_Base, DFT_CT, DFT_Base, WHT_Base, WHT_Base,  DFT_SplitRadix)//WHT_Base )//DFT_SplitRadix)
}