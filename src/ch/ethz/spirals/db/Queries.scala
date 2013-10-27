/**
 *     _______  _______  ___   __      ____     Automatic
 *    / __/ _ \/  _/ _ \/ _ | / /     / __/     * Implementation
 *   _\ \/ ___// // , _/ __ |/ /__   _\ \       * Optimization
 *  /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation
 *                                              of DSP Algorithms
 *  https://bitbucket.org/GeorgOfenbeck/spirals
 *  SpiralS 0.1 Prototype - ETH Zurich
 *  Copyright (C) 2013  Georg Ofenbeck (ofenbeck@inf.ethz.ch)
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

package ch.ethz.spirals.db

import java.io.{File, PrintStream, PrintWriter}
import scala.slick.driver.SQLiteDriver.simple._
import org.sqlite.SQLiteDataSource
import Database.threadLocalSession
import scala.util.parsing.combinator.JavaTokenParsers
import ch.ethz.spirals.rewrites.BreakdownRules
import ch.ethz.spirals.dsls.DFT


object Queries {
  /**
   * This function will query the database for all degrees of freedom (all possible breakdowns on one level) for a given
   * parent NT
   * @param parentID The parent NT id we would like to get all DF for
   * @return The filtered Query
   */
  def get_DFs(parentID : Int) =
  {
    DB().withSession {
    val df_all = for {
    df_child <- DF_applied_child
    df_parent <- df_child.parent
    cnt <- df_child.nt
    r  <- df_parent.ruleapplied
    rname <- r.rule
    pnt <- r.nt
  } yield (df_parent,pnt,cnt,r,rname,df_child)

    val df_filtered = df_all.filter(_._2.id === parentID)
    df_filtered
    }
  }


  private def get_all_Scores() =
  {
    DB().withSession {
    val x = for{
      res <- Results
      cfg <- res.resultcfg
      tree <- cfg.breakdown
      nt <- tree.nt
      score <- res.score
      scoretype <- score.scoretype
    } yield (nt,tree,score,scoretype,cfg,res, tree.linearized_tree)
    x
    }
  }
  def get_fastest(in_spl: ch.ethz.spirals.dsls.SPL, sort: Int): String =
  {
    val fastest = DB().withSession {
      val ntID = DB.addNT(in_spl)

      val all_scores = get_all_Scores()
      val filtered_scores = all_scores.filter(_._1.id === ntID).filter(_._4.id === 10).sortBy(_._3.score)//.groupBy(_._4.scoretype)
      val l3 = filtered_scores.list
      if (l3.isEmpty)
        return ""
      else
        ( l3.head._2._3)
    }
    fastest
  }

  def get_fastest_tsc(in_spl: ch.ethz.spirals.dsls.SPL, sort: Int): Double =
  {
    val fastest = DB().withSession {
      val ntID = DB.addNT(in_spl)

      val all_scores = get_all_Scores()
      val filtered_scores = all_scores.filter(_._1.id === ntID).filter(_._4.id === 10).sortBy(_._3.score)//.groupBy(_._4.scoretype)
      val l3 = filtered_scores.list
      if (l3.isEmpty)
        return -1
      else
        l3.head._3._3
    }
    fastest
  }

  def get_GenProfile(in_spl: ch.ethz.spirals.dsls.SPL, sort: Int)=
  {
    DB().withSession {
      val ntID = DB.addNT(in_spl)

      val all_scores = get_all_Scores()
      val filtered_scores = all_scores.filter(_._1.id === ntID).filter(_._4.id > 13)//.groupBy(_._4.scoretype)
      val fil_list = filtered_scores.list
      val grouped_by_cfg = fil_list.groupBy(_._6._2)


      var head = true
      for (res <- grouped_by_cfg)
      {
          println("%30s".format("RuleTree"))
          println("%30s".format(res._2(0)._7))
          for (score <- res._2)
          {
            if (score._4._2.contains("tsc"))
            {
              println("%15s".format(score._4._2))
              println("%15d".format( score._3._3.toInt))// / scala.math.pow(10,9).toInt))
            }
          }
      }
    }
  }

  def Profile2File(in_list: List[ch.ethz.spirals.dsls.SPL], filename: String)
  {
    def get_GenProfileString(in_spl: ch.ethz.spirals.dsls.SPL, sort: Int): String=
    {
      var string = ""
      DB().withSession {
        val ntID = DB.addNT(in_spl)

        val all_scores = get_all_Scores()
        val filtered_scores = all_scores.filter(_._1.id === ntID).filter(_._4.id > 13)//.groupBy(_._4.scoretype)
        val fil_list = filtered_scores.list
        val grouped_by_cfg = fil_list.groupBy(_._6._2)


        var head = true
        for (res <- grouped_by_cfg)
        {
          //println("%30s".format("RuleTree"))
          string += in_spl.toString
          //string += " %30s".format(res._2(0)._7)
          string += " %30s".format(res._2(0)._7)
          for (score <- res._2)
          {
            if (score._4._2.contains("tsc"))
            {
              //println("%15s".format(score._4._2))
              string += " %15d".format( score._3._3.toInt)// / scala.math.pow(10,9).toInt))
            }
          }
          string += "\n"
        }
      }
      string
    }


    val stream = new java.io.PrintWriter(new java.io.FileOutputStream(filename))

    //Print the Header
    DB().withSession {
      val ntID = DB.addNT(in_list(0))
      val all_scores = get_all_Scores()
      val filtered_scores = all_scores.filter(_._1.id === ntID).filter(_._4.id > 13)//.groupBy(_._4.scoretype)
      val fil_list = filtered_scores.list
      val grouped_by_cfg = fil_list.groupBy(_._6._2)
      for (res <- grouped_by_cfg)
      {
        //println("%30s".format("RuleTree"))
        stream.print("DFT")
        //string += in_spl.toString
        stream.print(" Ruletree")
        for (score <- res._2)
        {
          if (score._4._2.contains("tsc"))
          {
            stream.print(" %15s".format(score._4._2))
            //println()

          }
        }
        stream.println
      }
    }

    in_list.foreach(x => stream.print(get_GenProfileString(x,1)))

    stream.close()


  }

  def get_Scores(in_spl: ch.ethz.spirals.dsls.SPL, sort: Int)=
  {
    DB().withSession {
      val ntID = DB.addNT(in_spl)

      val all_scores = get_all_Scores()
      val filtered_scores = all_scores.filter(_._1.id === ntID ).filter(_._4.id < 13)//.groupBy(_._4.scoretype)

      val fil_list = filtered_scores.list

      val grouped_by_cfg = fil_list.groupBy(_._6._2)


      var head = true
      for (res <- grouped_by_cfg)
      {
        if (head) {
          print("%30s".format("RuleTree"))
          for (score <- res._2)
            print("%15s".format(score._4._2))
          head = false
          println()
        }
        print("%30s".format(res._2(0)._7))
        for (score <- res._2)
          print("%15d".format(score._3._3.toInt))
        println()

      }
    }
  }

  private val exhaustive_trees = new scala.collection.mutable.HashMap[Int, Int] //maps NT_ID to #of exhaustive trees

  /**
   * Small wrapper around get_nr_exhaustive_trees(parentID: Int)
   * @param in_spl The SPL object that will get translated into an ID in the DB and then forwared to the actual function
   * @return The number of trees
   */
  def get_nr_exhaustive_trees(in_spl: ch.ethz.spirals.dsls.SPL): Int = {
    DB().withSession {
      val ntID = DB.addNT(in_spl)

      ch.ethz.spirals.search.Search.StoreExhaustiveRuleTrees(in_spl, true)
      get_nr_exhaustive_trees(ntID)
    }
  }
  /**
   * Recursive function that will determine the # of Breakdown Options (== # of Ruletrees) given Recursive Breakdown
   * Uses the exhaustive_choices HashMap to cache the found values for parentID's
   * Assumes its called within a session
   * @param parentID The parent NT id we would like to get all choices for
   * @return The number of trees
   */
  private def get_nr_exhaustive_trees(parentID: Int): Int =
  {
      if (exhaustive_trees.contains(parentID)) //we found it in cache - so simply return
        exhaustive_trees.get(parentID).get

      val dfs = get_DFs(parentID)
      val l3 =  dfs.list
      if (l3.isEmpty) //where are at the bottom leafs of the tree
        return 1

      var choices = 0
      val grouped_by_rule = l3.groupBy(_._5._2) //group by Rulename

      //println(grouped_by_rule.first._1)

      for (per_rule <- grouped_by_rule)
      {
        val grouped_by_df = per_rule._2.groupBy(_._1._3) //group by DF
        for (per_df <- grouped_by_df)
        {
          var total = 1
          for (leaf <- per_df._2) //each leaf
          {
            val child_id = leaf._3._1
            total = total * get_nr_exhaustive_trees(child_id)
          }
          choices = choices + total
        }
        }
        exhaustive_trees += (parentID ->  choices )
        choices
   }


  def PrintExhaustiveTrees(in_spl: ch.ethz.spirals.dsls.SPL) : Unit =
  {
    val total_options = get_nr_exhaustive_trees(in_spl)

    for (i <- 0 until total_options)
    {
      println("-----------------------------------------")
      println("Tree Nr: "+i)
      PrintExhaustiveTree(in_spl,i)
    }
  }



  import scala.util.parsing.combinator._

  class LinTreeParser extends JavaTokenParsers {
    def bd_id : Parser[Any] = wholeNumber

  }


  def PrintLinearizedTree(lin_tree : String) : Unit =
  {
    /*DB().withSession {
      PrintLinearizedTree_recurse(lin_tree)
    }*/
  }

  def PrintLinearizedTree_recurse( lin_tree : String) : Unit =
  {

  }



  def PrintExhaustiveTree(in_spl: ch.ethz.spirals.dsls.SPL, index: Int) : Unit =
  {
    DB().withSession {
      val ntID = DB.addNT(in_spl)
      PrintExhaustiveTree_recurse(ntID,index,"")
    }
  }

  def getExhaustiveTrees(in_spl: ch.ethz.spirals.dsls.SPL)  =
  {
    val total_options = get_nr_exhaustive_trees(in_spl)
    for (i <- 0 until total_options)
    {
      println("-----------------------------------------")
      println("Tree Nr: "+i)
      val (x,linear_ruletree) = getExhaustiveTree(in_spl,i)
      //println(linear_ruletree)
    }
  }


  def getExhaustiveTree(in_spl: ch.ethz.spirals.dsls.SPL, index: Int): (ch.ethz.spirals.rewrites.BreakDown, Int)  =
  {
    get_nr_exhaustive_trees(in_spl)
    DB().withSession {
      val ntID = DB.addNT(in_spl)
      val (x,linear_ruletreex) = getExhaustiveTree_recurse(ntID,-1,index)    //-1 encoding no parent
      //val linear_ruletree = linear_ruletreex
      val linear_ruletree = Breakdown2LinTree(x)
      val ruletree_id = DB.addRuleTree(linear_ruletree,ntID)
      (x,ruletree_id)
    }
  }






  def RestoreBDfromLinearTree( lintree: String) = // : ch.ethz.spirals.rewrites.BreakDown =
  {

    import ch.ethz.spirals.rewrites._
    case class Node(a: Int, children: List[Node])

    object TreeParser extends JavaTokenParsers {
      def integer  = """(0|[1-9]\d*)""".r ^^ { _.toInt }
      def regularNode: Parser[List[Node]] = (
        integer ~ children ^^ { case num ~ list => List(Node(num, list)) }
          | integer ~ "()" ^^ { case num ~ list => List(Node(num, List())) }
        )
      def children: Parser[List[Node]] = "(" ~> repsep(regularNode,",") <~ ")"  ^^ (_.flatten)
      def parse(input: String) = parseAll(children, input) match {
        case Success(result, _) => result
        case NoSuccess(msg, _) => throw new RuntimeException("Parsing Failed:" + msg)
      }
    }


    val listing = TreeParser.parse(lintree)

    DB().withSession {

    def recurse (listing: List[Node]): List[BreakDown] =
    {

      val children_resolved = for (onebd <- listing) yield
      {
        val bddb = for {
          bd <- Breakdown
          upper_tail <- bd.parent
          upper_head <- upper_tail.parent
          rule_used <- upper_head.ruleapplied
          rule <- rule_used.rule
          upper_nt <- rule_used.nt


          lower_head <- bd.child
          lower_rule_used <- lower_head.ruleapplied
          lower_rule <- lower_rule_used.rule
          lower_nt <- lower_rule_used.nt
        } yield (bd.id,rule.id,upper_nt.id, lower_nt.id, lower_rule.id, lower_rule.rulename)
        val bddb_filtered = bddb.filter(_._1 === onebd.a)
        val res = bddb_filtered.list.head
        println(res)

        val nt = NT_id_2_SPL(res._4)


        if (!onebd.children.isEmpty) //we are not at F2 yet
          new BreakDown(nt,
            Some(
              new BreakDown_resolved(
                BreakdownRules.all(res._5),
                recurse (onebd.children)
              )))
        else{ //we reached F2
          //println(res._5)
          //println(nt.toString)
          val children: List[BreakDown] = BreakdownRules.all(res._5).apply(nt)
          children(0)
        }
      }
      children_resolved

      //new BreakDown(in_bd.nt,Some(new BreakDown_resolved(
      //  BreakdownRules.all(in_rule),children_resolved)))
    }

      //this is not so nice yet - doing a seperate query for the head
      val bddb = for {
        bd <- Breakdown
        upper_tail <- bd.parent
        upper_head <- upper_tail.parent
        rule_used <- upper_head.ruleapplied
        rule <- rule_used.rule
        upper_nt <- rule_used.nt
        lower_head <- bd.child
        lower_rule <- lower_head.ruleapplied
        lower_nt <- lower_rule.nt
      } yield (bd.id,rule.id,upper_nt.id, lower_nt.name, lower_nt.size)
      val bddb_filtered = bddb.filter(_._1 === listing(0).a)
      val res = bddb_filtered.list.head

      val nt = NT_id_2_SPL(res._3)
      val children_resolved = recurse(listing)
      new BreakDown(nt,
        Some(
          new BreakDown_resolved(
            BreakdownRules.all(res._2),
            children_resolved
          )
        )
      )

    }
  }



  def Breakdown2LinTree(in_bd : ch.ethz.spirals.rewrites.BreakDown ): String =
  {
    // The relationship BD to the Database mapping

    //---------------------------------------------------------------
    // BreakDown.nt                   <--> df.applied_parent
    //          .applied_bd                             .ruleappliedID
    //                     .rule                        .dfindex
    //                     .children (List)
    //In the breakdown we save the rule and a list of children
    //-> in the Database we save the rule and the index of the DF choice (e.g. first possible breakdown)








    def recurse (in_bd : ch.ethz.spirals.rewrites.BreakDown, parent: Int ) : String =
    {
      val parent_nt = in_bd.nt
      val children_bd = in_bd.applied_bd.get.children
      //val children_nt = children_bd map (x => x.nt)
      val rule = in_bd.applied_bd.get.rule
      val rule_index = ch.ethz.spirals.rewrites.BreakdownRules.all.indexOf(rule)
      //val possible_breakdowns = rule.apply(parent_nt)


      val ntid = DB.addNT(parent_nt)
      //check if we saved the rule already
      val ruleid = DB.addRule_applied(ntid,rule_index)
      //println("debug " + rule_index)

      //Check/Add the Head to the database
      //val bd_pos = possible_breakdowns.map (x => x.applied_bd.get.children)
      val bd_pos = BreakdownRules.GetBDDFIndex(in_bd)
      val upperid = DB.addDF_applied_parent(ruleid, bd_pos)
      //println("checking res " + bd_pos + " of " + possible_breakdowns.size)


      val returnstring =
        if (parent != -1)
        {
          val index = DB.addBreakDown(parent,upperid) //parent in this case is child from previous iteration
          index.toString
        }
        else
          ""

      var i = 0
      val child_strings: List[String] =
        for (child <- children_bd)
          yield
          {
            //Check/Add the children to the db
            val childid = DB.addDF_applied_child(upperid,
              i,
              //children_bd.indexOf(child),
              DB.addNT(child.nt))
            i = i+ 1
            //println("child id: " + childid + " index: " + i + " of " + children_bd.size)

            val child_string:String =
              if (child.applied_bd.isDefined)
                recurse(child, childid)
              else
                ""
            child_string
          }

     returnstring + "(" + child_strings.reduceLeft( _ + "," + _) + ")"

    }


      recurse(in_bd,-1)



  }

  //val x = List((2,List()),(3,List()))

  //val x = (5,List((7,List((1,List((2,List()),(3,List()))),(4,List()))),(8,List((2,List()),(3,List())))))

  def getExhaustiveTree_recurse(parentID: Int, upperDF: Int, index: Int): (ch.ethz.spirals.rewrites.BreakDown,String) =
  {
    import ch.ethz.spirals.dsls._


    val dfs = get_DFs(parentID)
    val l3 =  dfs.list
    if (l3.isEmpty) //where are at the bottom leafs of the tree
      F_2()
    var i = index
    val grouped_by_rule = l3.groupBy(_._5._2) //group by Rulename

    val parent_spl = NT_id_2_SPL(parentID)
    var dirty_return = new ch.ethz.spirals.rewrites.BreakDown(parent_spl,None)
    var return_string = ""
    var subtrees = ""

    //println(grouped_by_rule.first._1)

    for (per_rule <- grouped_by_rule)
    {
      val grouped_by_df = per_rule._2.groupBy(_._1._3) //group by DF
      for (per_df <- grouped_by_df)
      {
        var total_per_df = 1
        for (leaf <- per_df._2) //each leaf
        {
          val child_id = leaf._3._1
          total_per_df = total_per_df * get_nr_exhaustive_trees(child_id)
        }

        i = i - total_per_df

        if (i < 0) //its within that range
        { //then we select the current DF
        val q = Query(NT).filter(_.id === parentID)
        var remaining_index = total_per_df + i


          val root_df_id = per_df._2.head._1._1
          if (upperDF != -1)
          {
            val index = DB.addBreakDown(upperDF,root_df_id)
            return_string = index.toString
          }


          val children_resolved = for (leaf <- per_df._2) yield //each leaf
          {
            //in case of DFT8 -> DFT2 x DFT4
            //root_df_id is is id of DFT8
            //leaf_df_id is either 2 or 4
            //upperDF is the leaf_df_id from the previous iteration

            val leaf_df_id = leaf._6._1



            val child_id = leaf._3._1
            val nr = get_nr_exhaustive_trees(child_id)

            val (bd,child_tree) = getExhaustiveTree_recurse(child_id,leaf_df_id,remaining_index % nr)
            //println(x1._2.indexOf(x))
            //here we recurse down

            remaining_index = remaining_index / nr
            (bd,child_tree)
          }
          val strings = children_resolved.map( x => x._2)
          //subtrees = strings.reduceLeft("(" + _ +"),("+ _ + ")") //concat the strings
          subtrees = strings.reduceLeft( _ +","+ _ ) //concat the strings
          //val listofbreakdowns = BreakdownRules.all filter(_.isDefinedAt(parent_spl))

          val rule_id = Query(Rules).filter(_.rulename === per_rule._1)
          val rule_choice = ch.ethz.spirals.rewrites.BreakdownRules.all (rule_id.first._1)

          val possible_dfs = rule_choice(parent_spl)

          val bd = possible_dfs(per_df._1)

          val expanded_children = children_resolved.map( x => x._1)
          if (expanded_children.isEmpty)
            println("DEBUG - empty7!")
          import ch.ethz.spirals.rewrites._
          val bd_withchildren = new BreakDown(parent_spl,Some(new ch.ethz.spirals.rewrites.BreakDown_resolved(bd.applied_bd.get.rule,expanded_children)))
          i = 99999999 //fixme
          dirty_return = bd_withchildren
        }
      }
    }
    (dirty_return, return_string + "(" + subtrees + ")" )
  }


  def PrintExhaustiveTree_recurse(parentID: Int, index: Int, spacing: String): Unit =
  {
    val dfs = get_DFs(parentID)
    val l3 =  dfs.list
    if (l3.isEmpty) //where are at the bottom leafs of the tree
      println(spacing + "--> F2")

    var i = index
    val grouped_by_rule = l3.groupBy(_._5._2) //group by Rulename

    //println(grouped_by_rule.first._1)

    for (per_rule <- grouped_by_rule)
    {
      val grouped_by_df = per_rule._2.groupBy(_._1._3) //group by DF
      for (per_df <- grouped_by_df)
      {
        var total_per_df = 1
        for (leaf <- per_df._2) //each leaf
        {
          val child_id = leaf._3._1
          total_per_df = total_per_df * get_nr_exhaustive_trees(child_id)
        }

        i = i - total_per_df

        if (i < 0) //its within that range
        { //then we select the current DF
          val q = Query(NT).filter(_.id === parentID)
          println(spacing + "--> " + q.list.head._2 + "("  + q.list.head._3 + "," + q.list.head._4 + ")" + " @ (" +  per_rule._1 + ")" + per_df._1) // the Rule
          var remaining_index = total_per_df + i
          for (leaf <- per_df._2) //each leaf
          {
            val child_id = leaf._3._1
            val nr = get_nr_exhaustive_trees(child_id)

            //println(x1._2.indexOf(x))
            if (per_df._2.indexOf(leaf) == per_df._2.size-1)
              PrintExhaustiveTree_recurse(child_id, remaining_index % nr, spacing + "    ")
            else
              PrintExhaustiveTree_recurse(child_id, remaining_index % nr, spacing + "   |")
            remaining_index = remaining_index / nr
          }
          i = 99999999 //fixme
        }
      }
    }
  }


  def NT_id_2_SPL (id: Int) : ch.ethz.spirals.dsls.SPL =
  {
    import ch.ethz.spirals.dsls._
    val q = Query(NT).filter(_.id === id)

    q.first._2 match {
      case "DFT" => DFT(q.first._3,q.first._4)
      case "F2" => F_2()
      case _ =>
        assert(false, "NT not found in database")
        F_2() //replace by error
    }

  }

}

