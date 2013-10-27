/**
 *  SpiralS - ETH Zurich
 *  Copyright (C) 2013  Leo Büttiker   (leob@ethz.ch)
 *                      Georg Ofenbeck (ofenbeck@inf.ethz.ch)
 *                      Alen Stojanov  (astojanov@inf.ethz.ch)
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

package ch.ethz.spirals.rewrites.SigmaSPLTransformer
import scala.virtualization.lms.common.{NumericOpsExp, EffectExp}
import ch.ethz.spirals.dsls.{SigmaSPL_DSL}
import ch.ethz.spirals.db.DB
import ch.ethz.spirals.datatypes.DSLBaseTypes

trait SigmaSPLTransformer extends GraphTransformer{ self =>


  val IR: SigmaSPL_DSL
  val newIR: DSLBaseTypes with EffectExp

  import IR._

  import PatternSimplificator._
  import scalaz.{Tree,TreeLoc}

  def mergePrimePerm(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(Gather,"f",<(SPL_V,"r","s","alpha","beta", "next")>)>) ->
      (<(Gather,<(IM_Compose,<(IM_V, "s", "r", "r", "r")>,"f",getRangeOfIM("f"),getDomainOfIM("f"))>,"next")>) //Watch out for the exchanged params
      //range / domain for V not correct but not used later
    //(<(Gather,<(IM_Compose,<(IM_W, "r", "s")>,"f",getSizeOfIM("f"))>,"next")>)

  ).localAST

  def removeTagEndTagStart(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(Gather,"f",(<(TagEnd,<(TagStart, "body", "tag1")>,"tag2")>))>) ->
      (<(Gather,"f", "body")>)
      //(<(Gather,"f", <(SPL_I,0,"body")>)>)
  ).localAST


  def mergePrimePermt(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(SPL_Vt,"r","s","alpha","beta",
      (<(Sigma,"from","to","i",
        (<(Scatter,"f","next")>)
      )>)
    )>)
     ->
      (<(Sigma,"from","to","i",
        (<(Scatter,<(IM_Compose,<(IM_V, "s", "r", "r", "r")>,"f",getRangeOfIM("f"),getDomainOfIM("f"))>,"next")>) //Watch out for the exchanged params
      )>)
    //range / domain for V not correct but not used later
    //(<(Gather,<(IM_Compose,<(IM_W, "r", "s")>,"f",getSizeOfIM("f"))>,"next")>)
    ,
    (<(SPL_Vt,"r","s","alpha","beta",
      <(TagStart,
      (<(Sigma,"from","to","i",
        (<(Scatter,"f","next")>)
      )>),"tag"
      )>
    )>)
      ->
      (<(TagStart,
      (<(Sigma,"from","to","i",
        (<(Scatter,<(IM_Compose,<(IM_V, "s", "r", "r", "r")>,"f",getRangeOfIM("f"),getDomainOfIM("f"))>,"next")>) //Watch out for the exchanged params
      )>),"tag"
      )>)
     ,
    (<(SPL_Vt,"r","s","alpha","beta",
      <(TagEnd,
        (<(Sigma,"from","to","i",
          (<(Scatter,"f","next")>)
        )>), "tag"
      )>
    )>)
      ->
      (<(TagEnd,
        (<(Sigma,"from","to","i",
          (<(Scatter,<(IM_Compose,<(IM_V, "s", "r", "r", "r")>,"f",getRangeOfIM("f"),getDomainOfIM("f"))>,"next")>) //Watch out for the exchanged params
        )>), "tag"
      )>)



  ).localAST


  def mergeRaderPermsW(g:TreeLoc[Code]) = GraphTransformer(g).transform(
      (<(Gather,"f",<(SPL_W,"n","phi","g", "next")>)>) ->
      (<(Gather,<(IM_Compose,<(IM_W, "g", "n","n")>,"f",getRangeOfIM("f"),getDomainOfIM("f"))>,"next")>)
  ).localAST

  def mergeRaderPermsWT(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(SPL_Wt,"n","phi","g",
      (<(DirectSum,
        (<(Scatter,"fl", "nextl")>),
        //"b"
        (<(Scatter,"fr", "nextr")>)

      )>)
    )>)
    ->
      (<(DirectSum,
        <(Scatter,<(IM_Compose,<(IM_W, "g", "n", "n")>,"fl",getRangeOfIM("fl"),getDomainOfIM("fl"))>, "nextl")>,
       //"b"
        <(Scatter,<(IM_Compose,<(IM_W, "g", "n", "n")>,"fr",getRangeOfIM("fr"),getDomainOfIM("fr"))>, "nextr")>)
      >)
  ).localAST

  def mergeRaderMidLoopDependent(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(Gather,"im",
      (<(DirectSum,"a",
        (<(Scatter, "im_scatter",
          (<(SPL_RaderDiag,"n","k","root","next")>)
        )>)
      )>)
    )>)

    ->
      (<(SPL_S_RaderDiag, "n", "k", "root", "im",
      (<(Gather,"im",
        (<(DirectSum,"a",
          (<(Scatter, "im_scatter",
            (<(SPL_I,"n","next")>)
          )>)
        )>)
      )>)
      )>)

  ).localAST






  def mergePermsFun(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(Gather,"f",<(SPL_L,"n","k1", "next")>)>) ->
    (<(Gather,<(IM_Compose,<(IM_L,"k1",PatternDiv("n","k1"),1,1)>,"f",getRangeOfIM("f"),getDomainOfIM("f"))>,"next")>)
  ,
    (<(Gather,"f",<(TagEnd,<(SPL_L,"n","k1", "next")>,"tag")>)>) ->
      (<(Gather,<(IM_Compose,<(IM_L,"k1",PatternDiv("n","k1"),1,1)>,"f",getRangeOfIM("f"),getDomainOfIM("f"))>,<(TagEnd,"next","tag")>)>)
  ,
    (<(Gather,"f",<(TagStart,<(SPL_L,"n","k1", "next")>,"tag")>)>) ->
      (<(Gather,<(IM_Compose,<(IM_L,"k1",PatternDiv("n","k1"),1,1)>,"f",getRangeOfIM("f"),getDomainOfIM("f"))>,<(TagStart,"next","tag")>)>)
  ).localAST


  def mergeIML(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(IM_Compose,<(IM_L,"m","k","ignore1", "ignore2")>, <(IM_H,"fragsize",1,"range", "domain")>, "range1", "domain1")>) ->
      (<(IM_H,<(NumericDivide, "fragsize", "k")>,"m", "range", "domain")>)
    ,
    (<(IM_Compose,<(IM_L,"m","k","ignore1", "ignore2")>,
      <(IM_Compose,
        <(IM_H,"fragsize",1,"range", "domain")>,"b")>, "range1", "domain1")>)
    ->
      (<(IM_Compose,<(IM_H,<(NumericDivide, "fragsize", "k")>,"m", "range", "domain")>,"b", "range1", "domain")>)



  ).localAST


  def mergeHFun(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(IM_Compose,<(IM_H,"bx","sx","rangex", "domainx")>, <(IM_H,"b","s","range","domain" )>, "rangeo", "domaino")>) ->
      (<(IM_H,<(NumericPlus, "bx",
        <(NumericTimes,"b","sx")>
      )>
        ,<(NumericTimes,"sx","s")>, "range", "domainx")>)
        //,<(NumericTimes,"sx","s")>, "size2")>)
        //,<(NumericTimes,"sx","s")>, 1)>)
    /*(<(IM_Compose,<(IM_H,"bx","sx","size1")>, <(IM_H,"b","s","size2")>, "size")>) ->
      (<(IM_H,<(NumericPlus, "bx",
        <(NumericTimes,"b","sx")>
        )>
      ,<(NumericTimes,"sx","s")>, "size1")>) */

    /*(<(IM_Compose,<(IM_L,"m","k","sizeL")>, <(IM_H,"fragsize",1,"sizeH")>, "size")>) ->
      (<(IM_H,<(NumericDivide, "fragsize", "k")>,"m", "sizeL")>)
    ,
    (<(IM_Compose,<(IM_L,"m","k","sizeL")>, <(IM_Compose, <(IM_H,"fragsize",1,"sizeH")>,"b")>, "size")>) ->
      (<(IM_Compose,<(IM_H,<(NumericDivide, "fragsize", "k")>,"m", "sizeL")>,"b", "size")>)
        ,       */
  /*
		(<(IM_Compose,<(IM_H,"bx","sx","size1")>, <(IM_H,"b","s","size2")>, "size")>) ->
		(<(IM_H,<(NumericPlus, <(NumericTimes,"b","sx")>,"bx")>,<(NumericTimes,"sx","s")>, "size1")>) */
	/*,
		(<(IM_Compose,<(IM_Compose,"a","b")>,"c", "size")>) ->
    (<(IM_Compose,"a",<(IM_Compose,"b","c")>, "size")>) */


  ).localAST

  def mergeIMW(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(IM_Compose,<(IM_W,"g","rangex","domainx")>, <(IM_H,0,1,1,"domain")>, "rangeo","domaino")>) ->
      (<(IM_H,0,1,1,"domain")>)
    /*,

    (<(IM_Compose,<(IM_W,"g","size1")>, <(IM_H,1,1,"size2")>, "size")>) ->
      (<(IM_WT,1,"g","size1")>)
    */

  ).localAST

  def mergeIMW2(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(IM_Compose,<(IM_W,"g","rangex", "domainx")>, <(IM_H,1,1,"range", "domain")>, "rangeo", "domaino")>) ->
      (<(IM_WT,1,"g","range", "domain")>)
  ).localAST


  //rule 2.63
  def mergeIMWT(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(IM_Compose,<(IM_WT,"phi", "g","rangex", "domainx")>, <(IM_H,"b","s","range", "domain")>, "rangeo", "domaino")>) ->
      (<(IM_WT,
        (<(NumericTimes,"phi",
          (<(Pow,"g", "b")>)
        )>)
        ,
        (<(Pow,"g", "s")>)
        ,"range","domainx"
      )>)
  ).localAST

  def mergeIMWT2(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(IM_Compose,<(IM_WT,"phi", "g","rangex", "domainx")>, <(IM_Z,"b","s","range", "domain")>, "rangeo", "domaino")>) ->
      (<(IM_WT,
        (<(NumericTimes,"phi",
          (<(Pow,"g", "b")>)
        )>)
        ,
        (<(Pow,"g", "s")>)
        ,"range","domainx"
      )>)
  ).localAST


  def mergeIMZ1(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    //(<(IM_Compose,<(IM_V,"k","m","s")>, <(IM_H,"b",1,"size2")>, "size")>) ->
    (<(IM_Compose,(<(IM_Z, "bx","sx","rangex", "domainx")>), <(IM_H,"b","s","range", "domain")>, "rangeo", "domaino")>) ->
    (<(IM_Z,
      (<(NumericPlus,  "bx",
        (<(NumericTimes, "sx", "b")>)
      )>),
      (<(NumericTimes, "sx", "s")>),
      "range", "domainx")>)
  ).localAST

  def mergeIMZ2(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    //(<(IM_Compose,<(IM_V,"k","m","s")>, <(IM_H,"b",1,"size2")>, "size")>) ->
    (<(IM_Compose,(<(IM_Z, "bx","sx","rangex", "domainx")>), <(IM_Z,"b","s","range", "domain")>, "rangeo", "domaino")>) ->
      (<(IM_Z,
        (<(NumericPlus,  "bx",
          (<(NumericTimes, "sx", "b")>)
        )>),
        (<(NumericTimes, "sx", "s")>),
        "range", "domainx")>)
  ).localAST

  def mergeIMV1(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    //(<(IM_Compose,<(IM_V,"k","m","s")>, <(IM_H,"b",1,"size2")>, "size")>) ->
    (<(IM_Compose,<(IM_V,"m","k","rangex", "domainx")>, <(IM_H,"b",1,"range", "domain")>, "rangeo", "domaino")>) ->
      //(<(IM_Z, (<(NumericDivide, "b", "m")>),"m","range", "domain")>)
      (<(IM_Z, "b","k","range", "domain")>)

  ).localAST

  def mergeIMV2(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    //(<(IM_Compose,<(IM_V,"k","m","s")>, <(IM_H,"b",1,"size2")>, "size")>) ->
    (<(IM_Compose,<(IM_V,"m","k","rangex", "domainx")>, <(IM_H,"b","s","range", "domain")>, "rangeo", "domaino")>) ->
      (<(IM_Z,
        (<(NumericTimes,"b","k")>),
        "m","range", "domain")>)

  ).localAST


  def makeScalingLoopDependent(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(Gather,"f",<(SPL_T,"n","d","k", "next")>)>) ->
    //(<(SPL_S_T,"n","d","k",<(IM_Compose,<(IM_Twiddle,"d","n",getDomainOfIM("f"),getDomainOfIM("f"))>,"f",getRangeOfIM("f"),getDomainOfIM("f"))>,<(Gather,"f","next")>)>)
    (<(SPL_S_T,"n","d","k","f",<(Gather,"f","next")>)>)


  ,
    (<(Gather,"f",<(SPL_T3L,"n","d","k","next")>)>) ->
    (<(SPL_S_T3L,"n","d","k",<(IM_Compose,<(IM_Twiddle,"n","d",getRangeOfIM("f"),getDomainOfIM("f"))>,"f",getRangeOfIM("f"),getDomainOfIM("f"))>,<(Gather,"f","next")>)>)

    //These should be gone and handled automatically soon (moving through tags)
    ,
    (<(Gather,"f",<(TagStart,<(SPL_T,"n","d","k", "next")>,"tag")>)>) ->
      (<(SPL_S_T,"n","d","k","f",<(Gather,"f",<(TagStart,"next","tag")>)>)>)
    ,
    (<(Gather,"f",<(TagEnd,<(SPL_T,"n","d","k", "next")>,"tag")>)>) ->
      (<(SPL_S_T,"n","d","k","f",<(Gather,"f",<(TagEnd,"next","tag")>)>)>)
    ,

    (<(Gather,"f",<(TagStart,<(SPL_T3L,"n","d","k","next")>,"tag")>)>) ->
      (<(SPL_S_T3L,"n","d","k",<(IM_Compose,<(IM_Twiddle,"n","d",getRangeOfIM("f"),getDomainOfIM("f"))>,"f",getRangeOfIM("f"),getDomainOfIM("f"))>,<(Gather,"f",<(TagStart,"next","tag")>)>)>)
  ,

    (<(Gather,"f",<(TagEnd,<(SPL_T3L,"n","d","k","next")>,"tag")>)>) ->
      (<(SPL_S_T3L,"n","d","k",<(IM_Compose,<(IM_Twiddle,"n","d",getRangeOfIM("f"),getDomainOfIM("f"))>,"f",getRangeOfIM("f"),getDomainOfIM("f"))>,<(Gather,"f",<(TagEnd,"next","tag")>)>)>)
  ).localAST




  def moveScalingFun(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(Gather,"f1",<(SPL_S_T,"n","d","k","f","next")>)>) ->
    (<(SPL_S_T,"n","d","k", <(IM_Compose,"f","f1",getRangeOfIM("f1"),getDomainOfIM("f1"))>, (<(Gather,"f1","next")>))>)
  ,
    (<(Gather,"f1",<(SPL_S_RaderDiag,"n","k","root","f","next")>)>) ->
    (<(SPL_S_RaderDiag,"n","k","root", <(IM_Compose,"f","f1",getRangeOfIM("f1"),getDomainOfIM("f1"))>, (<(Gather,"f1","next")>))>)
  ,
    (<(Gather,"f1",<(SPL_S_T3L,"n","d","k","f","next")>)>) ->
    (<(SPL_S_T3L,"n","d","k", <(IM_Compose, "f1","f",getRangeOfIM("f1"),getDomainOfIM("f1"))>, (<(Gather,"f1","next")>))>)
  ,
    //These should be gone handled automatically soon (moving through tags)
  
    (<(Gather,"f1",<(TagEnd,<(SPL_S_T,"n","d","k","f","next")>,"tag")>)>) ->
      (<(SPL_S_T,"n","d","k", <(IM_Compose,"f","f1",getRangeOfIM("f1"),getDomainOfIM("f1"))>, (<(Gather,"f1",<(TagEnd,"next","tag")>)>))>)
    ,
    (<(Gather,"f1",<(TagEnd,<(SPL_S_RaderDiag,"n","k","root","f","next")>,"tag")>)>) ->
      (<(SPL_S_RaderDiag,"n","k","root", <(IM_Compose,"f","f1",getRangeOfIM("f1"),getDomainOfIM("f1"))>, (<(Gather,"f1",<(TagEnd,"next","tag")>)>))>)
    ,
    (<(Gather,"f1",<(TagEnd,<(SPL_S_T3L,"n","d","k","f","next")>,"tag")>)>) ->
      (<(SPL_S_T3L,"n","d","k", <(IM_Compose, "f1","f",getRangeOfIM("f1"),getDomainOfIM("f1"))>, (<(Gather,"f1",<(TagEnd,"next","tag")>)>))>)

    ,
      (<(Gather,"f1",<(TagStart,<(SPL_S_T,"n","d","k","f","next")>,"tag")>)>) ->
      (<(SPL_S_T,"n","d","k", <(IM_Compose,"f","f1",getRangeOfIM("f1"),getDomainOfIM("f1"))>, (<(Gather,"f1",<(TagStart,"next","tag")>)>))>)
    ,
    (<(Gather,"f1",<(TagStart,<(SPL_S_RaderDiag,"n","k","root","f","next")>)>)>) ->
      (<(SPL_S_RaderDiag,"n","k","root", <(IM_Compose,"f","f1",getRangeOfIM("f1"),getDomainOfIM("f1"))>, (<(Gather,"f1",<(TagStart,"next","tag")>)>))>)
    ,
    (<(Gather,"f1",<(TagStart,<(SPL_S_T3L,"n","d","k","f","next")>,"tag")>)>) ->
      (<(SPL_S_T3L,"n","d","k", <(IM_Compose, "f1","f",getRangeOfIM("f1"),getDomainOfIM("f1"))>, (<(Gather,"f1",<(TagStart,"next","tag")>)>))>)
  
  ).localAST

  def moveGatterFun(g:TreeLoc[Code]) = GraphTransformer(g).transform(

  (<(Gather,"innergf",<(Gather,"outergf","next")>)>) ->
    (<(Gather,<(IM_Compose,"outergf","innergf",getRangeOfIM("innergf"),getDomainOfIM("innergf"))>,"next")>)
  ,
    (<(Gather,"innergf",<(TagEnd,<(Gather,"outergf","next")>,"tag")>)>) ->
      (<(Gather,<(IM_Compose,"outergf","innergf",getRangeOfIM("innergf"),getDomainOfIM("innergf"))>,<(TagEnd,"next","tag")>)>)
    ,
    (<(Gather,"innergf",<(TagStart,<(Gather,"outergf","next")>,"tag")>)>) ->
      (<(Gather,<(IM_Compose,"outergf","innergf",getRangeOfIM("innergf"),getDomainOfIM("innergf"))>,<(TagStart,"next","tag")>)>)


  ).localAST

  def moveScatterFun(g:TreeLoc[Code]) = GraphTransformer(g).transform(
    (<(Scatter, "outer", <(Sigma,"from","to","i", <(Scatter,"inner","next")>)>)>) ->
    (<(Sigma,"from","to","i",<(Scatter,<(IM_Compose,"outer","inner",getRangeOfIM("outer"),getDomainOfIM("outer"))>,"next")>)>)
  ,
    (<(Scatter, "outer",<(TagEnd, <(Sigma,"from","to","i", <(Scatter,"inner","next")>)>,"tag")>)>) ->
      (<(TagEnd,<(Sigma,"from","to","i",<(Scatter,<(IM_Compose,"outer","inner",getRangeOfIM("outer"),getDomainOfIM("outer"))>,"next")>)>,"tag")>)
  ,
    
    (<(Scatter, "outer",<(TagStart, <(Sigma,"from","to","i", <(Scatter,"inner","next")>)>,"tag")>)>) ->
      (<(TagStart,<(Sigma,"from","to","i",<(Scatter,<(IM_Compose,"outer","inner",getRangeOfIM("outer"),getDomainOfIM("outer"))>,"next")>)>,"tag")>)
    
  
  ).localAST

  def placeholder(g:TreeLoc[Code]) = g
  def getRangeOfIM (f: String) = PatternGetArgument(f, "range")
  def getDomainOfIM (f: String) = PatternGetArgument(f, "domain")






  def transform_select[A](in: List[IR.Vector], out: IR.Vector, block:Block[A], rules: List[String]) : (List[newIR.Vector], newIR.Vector) = {

    inputs = in
    output = out

    val theGraph = GraphFactory(block)

    GraphUtil.subtrees = GraphUtil.subtrees.empty //just to make sure we have a fresh start
    theGraph.subTrees.foreach( x => GraphUtil.subtrees += x._1.id -> x._2)
    GraphUtil.tree2dotDigraph(theGraph.tree,"new_new"+".graph")



    val trans = GraphTransformer(theGraph.tree.loc)

    //val trans = GraphTransformer(theGraph)
    val newAST = trans.localAST

    val functions = List(

      "2.48_L"           -> mergePermsFun _ , //this rule by itself will fail horrible!!!!!
      "2.54"              -> mergeIML _ ,

      "2.48_W"     -> mergeRaderPermsW _,
      "2.48_Wt"   -> mergeRaderPermsWT _,

        "2.48_V"     -> mergePrimePerm _,
        "2.48_Vt"     -> mergePrimePermt _,
        "2.50_T->diag"    -> makeScalingLoopDependent _ , //this rules fails without the scaling!
        "2.50_RaderD"     -> mergeRaderMidLoopDependent _ ,
        "2.50"            -> moveScalingFun _ ,
        "2.46"           -> moveGatterFun _ ,
        "2.47"          -> moveScatterFun _,

        //Prime Factor rules
        "2.57"                    -> mergeIMV1 _ ,
        "2.58"                    -> mergeIMV2 _ ,
        "2.59"                    -> mergeIMZ1 _ ,
        "2.60"                    -> mergeIMZ2 _ ,

        //Rader rules
        "2.61"                  -> mergeIMW _ ,
        "2.62"                     -> mergeIMW2 _ ,
        "2.63"                  -> mergeIMWT _,



        "2.64"                  -> mergeIMWT2 _,
        "2.56"              -> mergeHFun _ ,

      "remove_TagEndTagStart" -> removeTagEndTagStart _,
      "placeholder" -> placeholder _
    )



    val ffunctions = if (rules != null) functions.filter ( x => rules.contains(x._1)) else functions

    var tree = newAST
    var root:TreeLoc[Code] = (
      for((name, f)  <- ffunctions) yield {
      tree = f(tree)
      //plotGraphs(tree,name)
      tree
    }).last

    GraphUtil.tree2dotDigraph(root.toTree, "new_new_endtree" + ".graph")
    val r = GraphUtil.tree2Instructions(root.toTree)
    new_output = newIR.getDSLVector(r.asInstanceOf[newIR.Exp[newIR.DSLType]])

    (new_inputs, new_output)

  }

  def transform[A](in: List[IR.Vector], out: IR.Vector, block:Block[A]) : (List[newIR.Vector], newIR.Vector) = {
    transform_select(in, out, block,null)

  }

}
