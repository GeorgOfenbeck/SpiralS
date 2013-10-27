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

// Use the implicit threadLocalSession
import scala.slick.driver.SQLiteDriver.simple._
import org.sqlite.SQLiteDataSource
import scala.slick.session.Database._
import ch.ethz.spirals.util.{Profiling, Measurment}

// Use the implicit threadLocalSession
import Database.threadLocalSession

object Rules extends Table[(Int, String, Int)]("Rules") {
    def id = column[Int]("ID",O.PrimaryKey)  //this is the position in the BreakDown.all List
    def rulename = column[String]("RuleName")
    def version = column[Int]("Version")
    // Every table needs a * projection with the same type as the table's type parameter
    def * = id ~ rulename ~ version
}

//This is only temporary since we only very limited types of NT atm - will be replaced with one more indirection
object NT extends  Table[(Int, String, Int,Int)]("NT") {
  def id = column[Int]("ID",O.PrimaryKey,O.AutoInc)
  def name = column[String]("Name")
  def size = column[Int]("Size")
  def k = column[Int]("k")

  def * = id ~ name ~ size ~ k
  def forInsert = name ~ size ~ k
}


object Rule_applied extends Table[(Int,Int,Int)]("Rule_applied"){
  def id =   column[Int]("ID",O.PrimaryKey,O.AutoInc)
  def ntID = column[Int]("NT_ID")
  def ruleID = column[Int]("Rule_ID")

  def * = id ~ ntID ~ ruleID
  def forInsert = ntID ~ ruleID

  //foreign key
  def nt = foreignKey("NT_FK", ntID, NT)(_.id)
  def rule = foreignKey("NT_FK", ruleID, Rules)(_.id)
}

object DF_applied_parent extends Table[(Int,Int,Int)]("DF_applied_parent"){
  def id =   column[Int]("ID",O.PrimaryKey,O.AutoInc)
  def ruleappliedID = column[Int]("rule_applied_ID")
  def dfindex = column[Int]("dfnr")        //saving which choice it was here

  def * = id ~ ruleappliedID ~dfindex
  def forInsert = ruleappliedID ~dfindex

  //foreign key
  def ruleapplied = foreignKey("NT_FK", ruleappliedID, Rule_applied)(_.id)
}

object DF_applied_child extends Table[(Int,Int,Int,Int)]("DF_applied_child"){
  def id =   column[Int]("ID",O.PrimaryKey,O.AutoInc)
  def parentID = column[Int]("Parent_ID")
  def leafindex = column[Int]("leafindex") //this is to keep track of position information
  def ntID = column[Int]("NT_ID")

  def * = id ~ parentID ~leafindex ~ntID
  def forInsert = parentID ~leafindex ~ntID

  //foreign key
  def parent = foreignKey("Parent_FK", parentID, DF_applied_parent)(_.id)
  def nt = foreignKey("NT_FK", ntID, NT)(_.id)
}


/**
 * This one is a bit confusing - the idea is that it links "Child" of one level
 * breakdown to the "Parent" of the next level - so the meanings of the words are a bit
 * counter intuitive
 */
object Breakdown extends Table[(Int,Int,Int)]("Breakdown"){
  def id = column[Int]("ID",O.PrimaryKey,O.AutoInc)
  def upperID = column[Int]("Upper")
  def lowerID = column[Int]("Lower")

  def * = id ~ upperID ~ lowerID
  def forInsert = upperID ~ lowerID

  //foreign key
  def parent = foreignKey("Parent_FK", upperID, DF_applied_child)(_.id)
  def child = foreignKey("Child_FK", lowerID, DF_applied_parent)(_.id)
}

object RuleTree extends Table[(Int,Int,String)]("RuleTree") {
  def id = column[Int]("ID",O.PrimaryKey,O.AutoInc)
  def linearized_tree = column[String]("Linearized_Tree")
  def ntID = column[Int]("NT_ID")
  def * = id ~ ntID ~linearized_tree
  def forInsert = ntID ~linearized_tree

  //foreign key
  def nt = foreignKey("NT_FK", ntID, NT)(_.id)
}




object System extends Table[(Int,Int)]("System"){
  def id = column[Int]("ID",O.PrimaryKey,O.AutoInc)
  def placeholder = column[Int]("Placeholder")
  def * = id ~ placeholder
  def forInsert = placeholder
}

object SearchConfig extends Table[(Int,Int)]("Config_Search"){
  def id = column[Int]("ID",O.PrimaryKey,O.AutoInc)
  def unroll_size = column[Int]("UnrollSize")
  def * = id ~ unroll_size
  def forInsert = unroll_size
}

object ScoreType extends Table[(Int,String)]("ScoreType")
{
  def id = column[Int]("ID",O.PrimaryKey,O.AutoInc)
  def scoretype = column[String]("ScoreType")
  def * = id ~ scoretype

  def forInsert = scoretype
}

object Score extends Table[(Int,Int,Double)]("Score"){
  def id = column[Int]("ID",O.PrimaryKey,O.AutoInc)
  def scoretypeid = column[Int]("ScoreType_ID")
  def score = column[Double]("Score")
  def * = id ~ scoretypeid ~ score
  def forInsert = scoretypeid ~ score
  //foreign key
  def scoretype = foreignKey("Scoretype_FK", scoretypeid, ScoreType)(_.id)

}

object ResultCfg extends Table[(Int,Int,Int,Int)]("Result_Config"){
  def id = column[Int]("ID",O.PrimaryKey,O.AutoInc)

  def systemid = column[Int]("SystemID")
  def searchconfigid = column[Int]("SearchConfigID")
  def ruletreeid = column[Int]("RuleTreeID")
  def * = id ~ systemid ~ searchconfigid ~ruletreeid
  def forInsert = systemid ~ searchconfigid ~ruletreeid
  //foreign key

  def system =foreignKey("System_FK",systemid, System)(_.id)
  def searchconfig =foreignKey("SearchConfig_FK",searchconfigid, SearchConfig)(_.id)
  def breakdown =foreignKey("RuleTree_FK",ruletreeid, RuleTree)(_.id)
}

object Results extends Table [(Int,Int,Int)]("Results") {
  def id = column[Int]("ID",O.PrimaryKey,O.AutoInc)
  def resultcfgID = column[Int]("ResultCfgID")
  def scoreid = column[Int]("ScoreID")

  def * = id ~ resultcfgID ~ scoreid
  def forInsert = resultcfgID ~ scoreid

  //foreign key
  def score = foreignKey("Score_FK", scoreid, Score)(_.id)
  def resultcfg = foreignKey("ResultCfg_FK", resultcfgID, ResultCfg)(_.id)
}



object SpiralSConfig extends Table[(Int,Boolean,Int,Boolean,Int, Boolean,String)]("SpiralSConfig"){
  def id = column[Int]("ID",O.PrimaryKey,O.AutoInc)
  def debug = column[Boolean]("Debug")
  def verbosity = column[Int]("Verbosity")
  def random = column[Boolean]("Random")
  def seed = column[Int]("Seed")
  def develop = column[Boolean]("Development")
  def comment = column[String]("Comment") //to make it easier to find specific comments

  def * = id ~ debug ~ verbosity ~ random ~  seed ~develop ~ comment
  def forInsert = debug ~ verbosity ~ random  ~seed~ develop  ~ comment
}


object CodeGenConfig extends Table[(Int,Boolean,Boolean,String)]("CodeGenConfig"){
  def id = column[Int]("ID",O.PrimaryKey,O.AutoInc)

  def generateComments = column[Boolean]("Generate_Comments")
  def usecodeformatter = column[Boolean]("Format_Code")
  def comment = column[String]("Comment") //to make it easier to find specific comments
  def * = id ~ generateComments ~usecodeformatter ~ comment
  def forInsert =  generateComments ~ usecodeformatter ~comment
}

object ValidationConfig extends Table[(Int,Boolean,Double,Int,Int,String)]("ValidationConfig"){
  def id = column[Int]("ID",O.PrimaryKey,O.AutoInc)

  def useFFTW = column[Boolean]("use_FFTW")
  def threshold = column[Double]("Threshold")
  def validationcycles = column[Int]("ValidationCycles")
  def compareMatricesThreshold = column[Int]("Compare_Matrices_Threshold")
  def comment = column[String]("Comment") //to make it easier to find specific comments
  def * = id ~ useFFTW ~threshold ~validationcycles ~ compareMatricesThreshold ~comment
  def forInsert =  useFFTW ~threshold ~validationcycles ~ compareMatricesThreshold ~comment
}



object CurrentConfig extends Table[(Int,Int,Int,Int)]("Current_Config"){
  def spiralsconfigID = column[Int]("SpiralSConfigID")
  def searchconfigID = column[Int]("SearchConfigID")
  def codegenconfigID = column[Int]("CodeGenConfigID")
  def validationconfigID = column[Int]("ValidationConfigID")
  //foreign key
  def spiralsconfig =foreignKey("SpiralConfig_FK",spiralsconfigID, SpiralSConfig)(_.id)
  def searchconfig =foreignKey("SearchConfig_FK",searchconfigID, SearchConfig)(_.id)
  def codegenconfig =foreignKey("CodeGenConfig_FK",searchconfigID, CodeGenConfig)(_.id)
  def validationconfig =foreignKey("ValidationConfig_FK",validationconfigID, ValidationConfig)(_.id)

  def * = spiralsconfigID ~ searchconfigID ~ codegenconfigID ~ validationconfigID
}




import org.sqlite.SQLiteConfig

object DB {
  import scala.slick.jdbc.meta._
  import scala.slick.driver.SQLiteDriver.simple._
  import scala.slick.session.Database.forDataSource

  val DBstring = "SpiralS.db" //TODO - push into SBT

  var db: scala.slick.session.Database = null

  def getConf () =
  {
    this.synchronized {
      this().withSession{
        val q = for {
          curr <- CurrentConfig
          spiralscfg <- curr.spiralsconfig
          searchcfg <- curr.searchconfig
          codecfg <- curr.codegenconfig
          validationcfg <- curr.validationconfig
        } yield(spiralscfg,searchcfg,codecfg,validationcfg)
        val x = q.list.head

        import ch.ethz.spirals._
        val config = conf.Configuration(
          conf.SpiralSConfig.tupled(x._1),
          conf.SearchConfig.tupled(x._2),
          conf.CodeGenConfig.tupled(x._3),
          conf.ValidationConfig.tupled(x._4)
        )
        config
      }
    }
  }

  def apply() = {
    if ( db == null ) {
      val ds = new SQLiteDataSource()
      ds.setUrl("jdbc:sqlite:"+DBstring)
      db = forDataSource(ds)
    }
    db
  }




  def ddl = (Rules.ddl ++ NT.ddl ++ Rule_applied.ddl ++ DF_applied_parent.ddl ++ DF_applied_child.ddl ++ Breakdown.ddl ++ System.ddl ++ SearchConfig.ddl ++ ScoreType.ddl ++ Score.ddl ++ Results.ddl ++
            SpiralSConfig.ddl ++ CurrentConfig.ddl ++ CodeGenConfig.ddl ++ ValidationConfig.ddl ++ RuleTree.ddl ++ ResultCfg.ddl
    )

  def checkCreate() =
  {
    this().withTransaction {


    val cur_size = MTable.getTables.list().size
    if (cur_size < 2) //create the Database if not exiting yet
    {
      ddl.create //create it

      //put minimum info inside
      Rules.insert(0,"WHT_CT",1)
      Rules.insert(1,"WHT_Base",1)
      Rules.insert(2,"DFT_CT",1)
      Rules.insert(3,"DFT_Base",1)
      Rules.insert(4,"DFT_Rader",1)
      Rules.insert(5,"DFT_GoodThomas",1)
      Rules.insert(6,"DFT_SplitRadix",1)

      ScoreType.insert(0,"perf_sd")
      ScoreType.insert(1,"perf_pd")
      ScoreType.insert(3,"perf_avxd")
      ScoreType.insert(4,"perf_ss")
      ScoreType.insert(5,"perf_ps")
      ScoreType.insert(6,"perf_avxs")

      ScoreType.insert(10,"perf_tsc")
      ScoreType.insert(11,"perf_doubles")
      ScoreType.insert(12,"perf_singles")


      ScoreType.insert(100,"CIR_Adds")
      ScoreType.insert(101,"CIR_Mults")



      ScoreType.insert(90000,"Generation_time_bd2spl")
      ScoreType.insert(90100,"Generation_time_emitSPLGraph")
      ScoreType.insert(90200,"Generation_time_emitSPLLatex")


      ScoreType.insert(91000,"Generation_time_SPL2SigmaSPL")
      ScoreType.insert(91100,"Generation_time_SigmaSPL2Block")
      ScoreType.insert(91200,"Generation_time_emitSigmaSPLGraph_before_rewrite")

      ScoreType.insert(91110,"Generation_time_SigmaSPL2Block_rewrite")
      ScoreType.insert(92000,"Generation_time_SigmaSPL_rewrite")
      ScoreType.insert(92100,"Generation_time_emitSigmaSPLGraph_after_rewrite")

      ScoreType.insert(93000,"Generation_time_CIR")
      ScoreType.insert(93001,"Generation_time_TwiddlePrecompute")
      ScoreType.insert(93100,"Generation_time_CIR2File")

      ScoreType.insert(94000,"Generation_time_Validation")

      ScoreType.insert(95000,"Generation_time_Perfplot")


      val searchid = SearchConfig.forInsert returning SearchConfig.id insert(5)
      val spiralsid = SpiralSConfig.forInsert returning SpiralSConfig.id insert(false,0,false,0,false,"Inital Config")
      val codegenid = CodeGenConfig.forInsert returning CodeGenConfig.id insert(true,true,"Inital Config")
      val validationid = ValidationConfig.forInsert returning ValidationConfig.id insert(false,1E-06,10,32,"Inital Config")
      CurrentConfig.insert(searchid,spiralsid,codegenid, validationid)
      System.forInsert insert(1)
    }     //ddl.drop


    }
  }
  def destroy = this().withTransaction{ddl.drop}



  import ch.ethz.spirals.dsls._
  def addNT(in_spl: SPL) : Int =
  {
    DB().withSession {
      val q = Query(NT)

      in_spl match{
        case DFT(n,k) =>
        {
          val q1 = q.filter(_.name === "DFT").filter(_.size === n).filter(_.k === k)
          if (q1.list.size == 0)
          {
            //NT.forInsert insert ("DFT",n,k)
            val Id = NT.forInsert returning NT.id insert ("DFT",n,k)
            return Id
          }
          else
            return q1.list.head._1
        }
        case F_2() =>
        {
          val q1 = q.filter(_.name === "F2")
          if (q1.list.size == 0)
          {
            //NT.forInsert insert ("DFT",n,k)
            val Id = NT.forInsert returning NT.id insert ("F2",0,0)
            return Id
          }
          else
            return q1.list.head._1
        }
        case _ => -1
      }
    }
  }

  def addCIRResult(adds: Int, mults : Int, resultcfgid: Int) : List[Int] =
  {
    val scoreids = Array(0,1)
    val resultids = Array(0,1)
    this().withTransaction {
      scoreids(0) = Score.forInsert returning Score.id insert (100,adds.toDouble)
      scoreids(1) = Score.forInsert returning Score.id insert (101,mults.toDouble)
      resultids(0) = Results.forInsert returning Results.id insert (resultcfgid,scoreids(0))
      resultids(1) = Results.forInsert returning Results.id insert (resultcfgid,scoreids(1))
    }
    resultids.toList
  }


  def addPerfplotResult(flop_list: List[Long], tsc : Long, resultcfgid: Int) : List[Int] =
  {

      val flops = flop_list.reduce(_+_)
      val scoreids = Array(0,1,2,3,4)
      val resultids = Array(0,1,2,3,4)

    this().withTransaction {
        scoreids(0) = Score.forInsert returning Score.id insert (0,flop_list(0).toDouble)
        scoreids(1) = Score.forInsert returning Score.id insert (1,flop_list(1).toDouble)
        scoreids(2) = Score.forInsert returning Score.id insert (2,flop_list(2).toDouble)
        //println("debug: " + flop_list(2))
        scoreids(3) = Score.forInsert returning Score.id insert (11,flops.toDouble)
        scoreids(4) = Score.forInsert returning Score.id insert (10,tsc.toDouble)

        resultids(0) = Results.forInsert returning Results.id insert (resultcfgid,scoreids(0))
        resultids(1) = Results.forInsert returning Results.id insert (resultcfgid,scoreids(1))
        resultids(2) = Results.forInsert returning Results.id insert (resultcfgid,scoreids(2))
        resultids(3) = Results.forInsert returning Results.id insert (resultcfgid,scoreids(3))
        resultids(4) = Results.forInsert returning Results.id insert (resultcfgid,scoreids(4))
    }
    resultids.toList
  }


  def addProfiling(profiling:  Profiling, resultcfgid: Int) =
  {
    this().withTransaction {
      for (measurment <- profiling.measurments)
      {
        val tsc_id = {
          //first check if we already have this type of key
          val full_table = for {
            scoretypes <- ScoreType
          } yield (scoretypes.id, scoretypes.scoretype)
          val filtered_table = full_table.filter(_._2 === (measurment.name + "_tsc") )
          val l3 = filtered_table.list

          val id = if (l3.isEmpty)
             ScoreType.forInsert returning ScoreType.id insert (measurment.name + "_tsc")
          else
            l3.head._1

          //now that we have the id - add to results
          val time = measurment.stoptime-measurment.starttime
          println("adding " + time + " -> " + measurment.name)
          val scoreid = Score.forInsert returning Score.id insert (id, time)
          Results.forInsert returning Results.id insert (resultcfgid,scoreid)
        }
        val mem_id = {
          //first check if we already have this type of key
          val full_table = for {
            scoretypes <- ScoreType
          } yield (scoretypes.id, scoretypes.scoretype)
          val filtered_table = full_table.filter(_._2 === (measurment.name + "_mem") )
          val l3 = filtered_table.list

          val id = if (l3.isEmpty)
            ScoreType.forInsert returning ScoreType.id insert (measurment.name + "_mem")
          else
            l3.head._1

          //now that we have the id - add to results
          val mem = measurment.stopmemory-measurment.startmemory
          val scoreid = Score.forInsert returning Score.id insert (id, mem)
          Results.forInsert returning Results.id insert (resultcfgid,scoreid)
        }
      }
    }
  }


  def addResultCfg(treeid: Int, systemid: Int, searchconfigid: Int): Int =
  {
    this().withTransaction {
      val q = Query(ResultCfg)
      val q1 = q.filter(_.ruletreeid === treeid).filter(_.systemid === systemid).filter(_.searchconfigid === searchconfigid)
      if (q1.list.size == 0)
      {
        //println("Debug: Inserting DF_applied")
        //systemid ~ searchconfigid ~ruletreeid
        ResultCfg.forInsert returning ResultCfg.id insert (systemid,searchconfigid,treeid)
      }
      else
        return q1.list.head._1
    }
  }


  def addRuleTree(tree : String , ntID: Int) : Int =
  {
    this().withTransaction {
      val q = Query(RuleTree)
      val q1 = q.filter(_.linearized_tree === tree)
      if (q1.list.size == 0)
      {
        //println("Debug: Inserting DF_applied")
        RuleTree.forInsert returning RuleTree.id insert (ntID,tree)
      }
      else
        return q1.list.head._1
    }
  }


  def addBreakDown(upper: Int, lower: Int ) : Int =
  {
    this().withTransaction {
    val q = Query(Breakdown)
    val q1 = q.filter(_.lowerID === lower).filter(_.upperID === upper)
    if (q1.list.size == 0)
    {
      //println("Debug: Inserting DF_applied")
      Breakdown.forInsert returning Breakdown.id insert (upper,lower)
    }
    else
      return q1.list.head._1
    }
  }

  def addDF_applied_parent(ruleid: Int, dfnr: Int) : Int =
  {
    this().withTransaction {
    val q = Query(DF_applied_parent)
    val q1 = q.filter(_.ruleappliedID === ruleid).filter(_.dfindex === dfnr)
    if (q1.list.size == 0)
    {
      println("Debug: Inserting DF_applied_parent")
      DF_applied_parent.forInsert returning DF_applied_parent.id insert (ruleid,dfnr)
    }
    else
      return q1.list.head._1
    }

  }
  def addDF_applied_child(parentid: Int, leaf: Int, ntid: Int) : Int =
  {
    this().withTransaction{
      val q = Query(DF_applied_child)
      val q1 = q.filter(_.leafindex === leaf).filter(_.ntID === ntid).filter(_.parentID === parentid)
      if (q1.list.size == 0)
      {
        println("Debug: Inserting DF_applied_child")
        DF_applied_child.forInsert returning DF_applied_child.id insert (parentid,leaf,ntid)
      }
      else
        return q1.list.head._1
    }
  }



  def addRule_applied(ntid: Int,rule_index: Int) : Int =
  {
    this().withTransaction{
    val q = Query(Rule_applied)
    val q1 = q.filter(_.ntID === ntid).filter(_.ruleID === rule_index)
    if (q1.list.size == 0)
    {
      Rule_applied.forInsert returning Rule_applied.id insert(ntid,rule_index)
    }
    else
      return q1.list.head._1
    }
  }






}



