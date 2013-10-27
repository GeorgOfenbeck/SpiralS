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

package ch.ethz.spirals.search

import ch.ethz.spirals.rewrites._
import ch.ethz.spirals.dsls._

import scala.slick.driver.SQLiteDriver.simple._
import Database.threadLocalSession
import ch.ethz.spirals.db._
import scala.collection.mutable
import scala.util.Random
import ch.ethz.spirals.conf.Config


/**
 * This object collects all available search engines
 */
object Search {
  /**
   * Fills the database with the exhaustive Tree of possible Breakdowns for "in_spl" (e.g. DFT(16))
   * Does not perform timing, therefore mainly used for debugging
   * @param in_spl
   */
  def StoreExhaustiveRuleTrees(in_spl: SPL, withinSession: Boolean = false) = {


    def recurse (in_spl: SPL): Unit =
    {
      val ntid = DB.addNT(in_spl)
      //add NT
      val applicable = BreakdownRules.all filter (_.isDefinedAt(in_spl))
      for (rule_choice <- applicable)
      {
        val rule_index = BreakdownRules.all.indexOf(rule_choice)
        //add Ruleapplied
        val ruleid = DB.addRule_applied(ntid,rule_index)

        val possible_breakdowns = rule_choice.apply(in_spl)

        for (bd <- possible_breakdowns) // all degres of freedom for that rule
        {
          bd.applied_bd map ( bd_existing => bd_existing.children map (child =>
          {
            val dfindex = DB.addDF_applied_parent(ruleid,
              possible_breakdowns.indexOf(bd))

            DB.addDF_applied_child(dfindex,
              bd_existing.children.indexOf(child),
              DB.addNT(child.nt))

            recurse(child.nt)
          }
            ))
        }
      }
    }

    if ( withinSession ) {
      recurse(in_spl)
    } else {
      DB().withTransaction {
        recurse(in_spl)
      }
    }
  }


var scoreMap =mutable.HashMap[ SPL, (BreakDown, Double)]()
//overwrite = true will retime
def DP (in_spl : SPL, retime_top: Boolean = false): BreakDown = {

  val searchconfigid = ch.ethz.spirals.conf.Config().searchconfig.id
  //val searchconfigid = 0
  val systemid = 0 //placeholder


  println("--------------------------------------------------------------------------")
  println("DP for " + in_spl)
  def recurse (in_spl: SPL, retime: Boolean = false): BreakDown =
  {
    val (in_spl_k1, korig) = in_spl match {  //to speed up search we use this and assume that DFT(n,k) is the same in speed as DFT(n,1)
      case DFT(n,k) => (DFT(n,1),k)
      case _ => (in_spl,0)
    }
    println("recurse for " + in_spl)
    if (scoreMap.contains(in_spl)) // we already have a score for that size and a Breakdown
    {
      println("looking up hash" + in_spl)
      scoreMap.get(in_spl).get._1
    }
    else
    if(!retime && Queries.get_fastest(in_spl_k1,1) != "") //check if we have something in the DB
    {
      println("found Breakdown in Database")
       val str = Queries.get_fastest(in_spl_k1,1)
      println("restoring: " + str)
       val bd = Queries.RestoreBDfromLinearTree(str)
      val corrected_k = BreakdownRules.correct_k(bd,in_spl)
      corrected_k
    }
    else
    {
      println("did not find Breakdown in Database")
    var bestscore : Double = Double.MaxValue
    var bestbreakdown: BreakDown = new BreakDown(in_spl,None)
    val ntid = DB.addNT(in_spl)
    //add NT
    val applicable = BreakdownRules.all filter (_.isDefinedAt(in_spl))
    for (rule_choice <- applicable)
    {
      val rule_index = BreakdownRules.all.indexOf(rule_choice)
      //add Ruleapplied
      val ruleid = DB.addRule_applied(ntid,rule_index)

      val possible_breakdowns = rule_choice.apply(in_spl)

      for (bd <- possible_breakdowns) // all degres of freedom for that rule
      {
        val expanded_children = bd.applied_bd map ( bd_existing => bd_existing.children map (child =>
        {
          val dfindex = DB.addDF_applied_parent(ruleid,
            possible_breakdowns.indexOf(bd))

          DB.addDF_applied_child(dfindex,
            bd_existing.children.indexOf(child),
            DB.addNT(child.nt))

          recurse(child.nt)
        }
          ))

        val bd_withchildren = new BreakDown(bd.nt,Some(new BreakDown_resolved(bd.applied_bd.get.rule,expanded_children.get)))
        //here we have on particular breakdown

        import ch.ethz.spirals.scoring._
        val score = if (bd.nt.size != 2)
        {
          println("getting score for: ")
          println(bd_withchildren.toString)
          val (tsc: Long,flops, profiling) = Scoring.getScore(bd_withchildren)


          val linear_ruletree = Queries.Breakdown2LinTree(bd_withchildren)
          //val linear_ruletree = "just checking"


            val ntID = DB.addNT(bd_withchildren.nt)
            val ruletree_id = DB.addRuleTree(linear_ruletree,ntID)
            val resultcfgid = DB.addResultCfg(ruletree_id,systemid,searchconfigid)
            DB.addPerfplotResult(flops,tsc,resultcfgid)
            DB.addProfiling(profiling,resultcfgid)



          tsc
        }
        else
          1

        if (score < bestscore)
        {
          bestscore = score
          bestbreakdown = bd_withchildren
        }
      }
    }

    scoreMap += ( in_spl ->  (bestbreakdown,bestscore) )
    bestbreakdown
    }
  }
  DB().withTransaction {
    recurse(in_spl, retime_top)
  }
/*
  if ( withinSession ) {
    recurse(in_spl)
  } else {
    DB().withTransaction {
      recurse(in_spl)
    }
  }*/
}

  def RandomTree(in_spl: SPL) = {
    val nr_trees = Queries.get_nr_exhaustive_trees(in_spl)
    val searchconfigid = ch.ethz.spirals.conf.Config().searchconfig.id

    val random = new Random(Config().spiralsconfig.seed)

    val i = random.nextInt(nr_trees)

      println("-----------------------------------------")
      println("Tree Nr: "+i)
      val (bd,lin_tree_id) = Queries.getExhaustiveTree(in_spl,i)
      //SpiralS. CIR_DSL_Object.codegen.print_struct = true
      val systemid = 0 //placeholder


      import ch.ethz.spirals.scoring._
      val (tsc,flops, profiling) = Scoring.getScore(bd)


      DB().withSession {
        val resultcfgid = DB.addResultCfg(lin_tree_id,systemid,searchconfigid)
        DB.addPerfplotResult(flops,tsc,resultcfgid)
        DB.addProfiling(profiling,resultcfgid)
      }

    //scores.map(x => println(x))
  }



def ExhaustiveSearch(in_spl: SPL) = {
  val nr_trees = Queries.get_nr_exhaustive_trees(in_spl)
  val searchconfigid = ch.ethz.spirals.conf.Config().searchconfig.id
  val scores = for (i <- 0 until nr_trees) yield
  {
    println("-----------------------------------------")
    println("Tree Nr: "+i)
    val (bd,lin_tree_id) = Queries.getExhaustiveTree(in_spl,i)
    //SpiralS. CIR_DSL_Object.codegen.print_struct = true
    val systemid = 0 //placeholder


    import ch.ethz.spirals.scoring._
    val (tsc,flops, profiling) = Scoring.getScore(bd)


      DB().withSession {
        val resultcfgid = DB.addResultCfg(lin_tree_id,systemid,searchconfigid)
        DB.addPerfplotResult(flops,tsc,resultcfgid)
        DB.addProfiling(profiling,resultcfgid)
      }
  }
  //scores.map(x => println(x))
  }



}





/*
  def DynamicProgramming(in_spl: SPL): BreakDown = {
  TODO
  }
  */

/*
def ExhaustiveSearch(in_spl: SPL) = {
  StoreExhaustiveRuleTrees(in_spl) //first fill the DB
  val index = DB.addNT(in_spl) //return the index of the NT
  val total_options = recurse(index)

  for (i <- 0 until total_options)
  {
    println("-----------------------------------------")
    println("Tree Nr: "+i)
    //TreeNR( index, i, "")
  }


  def getdfs() =
  {
    val df_all = for {
      df <- DF_applied
      cnt <- df.nt
      r  <- df.ruleapplied
      rname <- r.rule
      pnt <- r.nt
    } yield (pnt.name,pnt.id,rname.rulename,cnt.name,cnt.id,df.dfindex,df.leafindex)
    df_all
  }
  def recurse (ntID : Int): Int =
  {
    if (exhaustive_choices.contains(ntID))
      exhaustive_choices.get(ntID).get

    val dfs = getdfs().filter(_._2 === ntID) //where parent == in_spl
    val l3 =  dfs.list
    if (l3.isEmpty) //where are at the bottom leafs of the tree
      return 1;
    var choices = 0
    val grouped = l3.groupBy(_._3) //group by Rulename
    for (y <- grouped)
    {
      val z = y._2.groupBy(_._6) //group by DF
      for (x1 <- z)
      {
        var total = 1
        for (x <- x1._2) //each leaf
        {
          total = total * recurse(x._5)
        }
        choices = choices + total
      }
    }
    exhaustive_choices += (ntID ->  choices )
    choices
  }


}


val exhaustive_choices = new scala.collection.mutable.HashMap[Int, Int] //maps NT_ID to #of exhaustive trees
def PrintExhaustiveTrees(in_spl: SPL) = {

  DB().withSession {
    def getdfs() =
    {
      val df_all = for {
        df <- DF_applied
        cnt <- df.nt
        r  <- df.ruleapplied
        rname <- r.rule
        pnt <- r.nt
      } yield (pnt.name,pnt.id,rname.rulename,cnt.name,cnt.id,df.dfindex,df.leafindex)
      df_all
    }

    def TreeNR( pntID: Int, index : Int, spacing : String): Unit =
    {
      val dfs = getdfs().filter(_._2 === pntID) //where parent size == size
      val l3 =  dfs.list


      if (dfs.list.isEmpty)
        println(spacing + "--> F2")
      //val nr_options = recurse(size)

      var i = index
      val grouped = l3.groupBy(_._3) //group by Rulename
      for (y <- grouped)
      {
        val z = y._2.groupBy(_._6) //group by DF
        for (x1 <- z)
        {
          var total_per_df = 1
          for (x <- x1._2) //each leaf
          {
            total_per_df = total_per_df * recurse(x._5)
          }

          i = i - total_per_df
          //println(spacing +  y._1) // the Rule
          //println(spacing + x1._1) // the DF


          if (i < 0) //its within that range
          { //then we select the current DF
            val q = Query(NT).filter(_.id === pntID)
            println(spacing + "--> " + q.list.head._2 + "("  + q.list.head._3 + "," + q.list.head._4 + ")" + " @ (" +  y._1 + ")" + x1._1) // the Rule
            //println(spacing)


            var remaining_index = total_per_df + i
            for (x <- x1._2) //each leaf
            {
              val nr = recurse(x._5)

              //println(x1._2.indexOf(x))
              if (x1._2.indexOf(x) == x1._2.size-1)
                TreeNR(x._5, remaining_index % nr, spacing + "    ")
              else
                TreeNR(x._5, remaining_index % nr, spacing + "   |")
              remaining_index = remaining_index / nr
            }
            i = 99999999 //fixme
          }


        }
      }

      /*
      val nr_trees = recurse(x._5) //How many trees can we have
      TreeNR(x._5,cur_index%nr_trees) //x._5 is the size of the leaf
      cur_index = cur_index / nr_trees
        */
    }

    def recurse (ntID : Int): Int =
    {
      if (exhaustive_choices.contains(ntID))
        exhaustive_choices.get(ntID).get

      val dfs = getdfs().filter(_._2 === ntID) //where parent == in_spl
      val l3 =  dfs.list
      if (l3.isEmpty) //where are at the bottom leafs of the tree
        return 1;
      var choices = 0
      val grouped = l3.groupBy(_._3) //group by Rulename
      for (y <- grouped)
      {
        val z = y._2.groupBy(_._6) //group by DF
        for (x1 <- z)
        {
          var total = 1
          for (x <- x1._2) //each leaf
          {
            total = total * recurse(x._5)
          }
          choices = choices + total
        }
      }
      exhaustive_choices += (ntID ->  choices )
      choices
    }



    StoreExhaustiveRuleTrees(in_spl) //first fill the DB
    val index = DB.addNT(in_spl) //return the index of the NT
    val total_options = recurse(index)

    for (i <- 0 until total_options)
    {
      println("-----------------------------------------")
      println("Tree Nr: "+i)
      TreeNR( index, i, "")
    }


  }
}       */
/*
def NrOfExhaustiveTrees(in_spl: SPL) : Int = {
  DB().withSession {
    def getdfs(size: Int) =
    {
      val df_all = for {
        df <- DF_applied
        cnt <- df.nt
        r  <- df.ruleapplied
        rname <- r.rule
        pnt <- r.nt
      } yield (pnt.name,pnt.size,rname.rulename,cnt.name,cnt.size,df.dfindex,df.leafindex)
      df_all
    }

    def recurse (size: Int): Int =
    {
      val dfs = getdfs(size).filter(_._2 === size) //where parent size == size

      val l3 =  dfs.list


      if (l3.isEmpty) //where are at the bottom leafs of the tree
        return 1;

      var choices = 0
      val grouped = l3.groupBy(_._3) //group by Rulename
      for (y <- grouped)
      {
        val z = y._2.groupBy(_._6) //group by DF
        for (x1 <- z)
        {

          var total = 1
          for (x <- x1._2) //each leaf
          {
            total = total * recurse(x._5)
          }
          choices = choices + total
        }
      }
      choices
    }
    val size = in_spl.size
    recurse(size)
  }
}
*/







