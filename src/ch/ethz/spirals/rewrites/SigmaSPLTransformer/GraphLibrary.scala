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

import ch.ethz.spirals.dsls._
import scalaz.{Tree, TreeLoc, Foldable, Show}
import scala.collection.immutable.HashMap
import scala.reflect.SourceContext
import scala.virtualization.lms.common.{NumericOpsExp, EffectExp}
import ch.ethz.spirals.datatypes.DSLBaseTypes

trait GraphLibrary {  self =>

  val IR: DSLBaseTypes with NumericOpsExp
  val newIR: DSLBaseTypes with EffectExp

  protected var inputs : List[IR.Vector] = null
  protected var output : IR.Vector = null

  protected var new_inputs : List[newIR.Vector] = List.empty[newIR.Vector]
  protected var new_output : newIR.Vector = null

  object Reflection extends Reflection {
    val IR: self.newIR.type = newIR
  }

  /*
   * Extended Tree that offers more traverseal posibilities
   */
  case class ExtendedTree[A](tree:Tree[A]) {
    import scalaz.std.stream.{streamInstance, streamMonoid}
    import tree._
    /** Breadth-first traversal. */
    def treeLevels: Stream[Stream[Tree[A]]] = {
      val f = (s: Stream[Tree[A]]) => {
        Foldable[Stream].foldMap(s)((_: Tree[A]).subForest)
      }
      Stream.iterate(Stream(tree))(f) takeWhile (!_.isEmpty)
    }
  }

  //Tree Labels for use in scalaz.Tree
  abstract class Code {
    import IR.{Sym,Exp}
    var sym:Option[Sym[_]] = None
    def setSymAnnotation(s:Sym[_]) = {sym = Some(s);this}
    //TODO check if this is realy needed
    var originalRef:Option[AnyRef] = None
    def setRef(r:AnyRef) = {originalRef = Some(r);this}
    def toLeaf():Tree[Code] = Tree[Code](this);
  }



  //case class CS(loc : TreeLoc[Code] ) extends Code
  case class CSX(ref: Int ) extends Code
  case class ManifestCode(mev: AnyRef) extends Code
  case class NumericCode(aev: AnyRef) extends Code
  case class SourceContextCode(pos: AnyRef) extends Code

  case class Instruction(operand: AnyRef) extends Code
  case class BigCompose extends Code
  case class VariableX(instr:IR.Sym[_]) extends Code {
    setSymAnnotation(instr)
    override def toString():String = "Ref: "+instr
  }
  object CodeBlock extends Code

  abstract class Constant extends Code
  case class VectorConstant(v:IR.Exp[IR.Vector]) extends Constant
  case class IntegerConstant(i:Int, constEncapsulated: Boolean = true) extends Constant {
    override def toString():String = i.toString
  }
  case class BooleanConstant(b:Boolean) extends Constant {
    override def toString() :String = b.toString
  }
  implicit val CodeShow = Show.showFromToString[Code]

  // Generate a simple graph
  case class GraphFactory[A](block:IR.Block[A]) {
    import scala.virtualization.lms.internal.NestedBlockTraversal
    import IR.{Sym, TP, Block, Const, DefMN}

    val instr = IR.globalDefs.toList
    object Collector extends NestedBlockTraversal {
      val IR: self.IR.type = self.IR
      import IR._
      var instr:List[Stm] = List()

      override def traverseStm(stm: Stm): Unit = {
        instr ::= stm
        blocks(stm.rhs) foreach traverseBlock
      }
    }
    Collector.traverseBlock(block)
    val allGlobalDefs:List[IR.Stm] = Collector.instr.sortBy{ case TP(Sym(s),_) => s}

    var subTrees = Map[Sym[_], Tree[Code]]()
    var usedsubTrees = Map[Sym[_], Tree[Code]]()



    var instrLHS:Set[Sym[_]] = Set()
    var params:Set[Any] = Set()
    //FIRST PASS
    allGlobalDefs foreach {
      case TP(s:Sym[_], node: Product) =>
        instrLHS += s
        params |= node.productIterator.toSet
    }

    //SECOND PASS
    var usedSymbols:Set[Sym[_]] = Set()
    def findUsedSymbols(p:List[Any]):Unit = p foreach {
      case s:Sym[_] => usedSymbols += s
      case Block(s:Sym[_]) => usedSymbols += s
      case Const(node:Product) => findUsedSymbols(node.productIterator.toList)
      case _ =>
    }
    findUsedSymbols(params.toList)
    val externalSymbols:Set[Sym[_]] = usedSymbols -- instrLHS

    def paramToTree(p:Any):Tree[Code] = p match {
      case ma:Manifest[_] => ManifestCode(ma).toLeaf()
      case nu:Numeric[_] => NumericCode(nu).toLeaf()
      case sc:SourceContext => SourceContextCode(sc).toLeaf()
      case s:Sym[_] if(externalSymbols(s)) => val node = VariableX(s); subTrees += s -> node.toLeaf();subTrees(s)
      case s:Sym[_] => {
        //subTrees(s)
        if (usedsubTrees.contains(s) )
        {
          //println("Label")
          //println(subTrees(s).rootLabel.sym)
          CSX(subTrees(s).rootLabel.sym.get.id).toLeaf()
        }
        else
        {
          usedsubTrees += s -> subTrees(s)
          subTrees(s)
        }
      }
      case i:Int => IntegerConstant(i, false).toLeaf
      case Const(i:Int) => IntegerConstant(i).toLeaf
      case Block(s:Sym[_]) => Tree.node(CodeBlock, Stream(subTrees(s)))
      case Const(node:Product) => {
        createTreeNode(node,node.productIterator.toList)
      }
      case node:Product => createTreeNode(node, node.productIterator.toList)
    }

    //TODO symbol of Int, should be symbol of ?
    def createTreeNode(node:Product, params:List[Any], sym:Option[Sym[_]] = None):Tree[Code] = {
      val operand = Reflection.getCompanionObject(node)
      val instr = Instruction(operand)
      sym match{ case Some(s) => instr.setSymAnnotation(s); case _ => }
      Tree.node(instr, (params map paramToTree).toStream)
    }

    //THIRD PASS generate tree
    allGlobalDefs foreach {
      case TP(s:Sym[_],node:DefMN[_]) => {
        //we assume that every implementation of DefMN is a case class, therefore its save to cast to a Product
        val productNode = node.asInstanceOf[Product]
        var nodeParams = productNode.productIterator.toList ::: List(node.aev, node.mev, IR.mpos(s.pos))
        val t = createTreeNode(productNode, nodeParams,Some(s))
        subTrees += s -> t
      }



      case TP(s:Sym[_],node:Product) => {
        var nodeParams = node.productIterator.toList
        val t = createTreeNode(node, nodeParams,Some(s))
        subTrees += s -> t
      }
    }

    val graphRootSym = allGlobalDefs(allGlobalDefs.length-1) match { case TP(s:Sym[_],_) => s}
    val tree = subTrees(graphRootSym)
  }

  object TreeUtil {
    def findAll[A](t:TreeLoc[A], p:TreeLoc[A] => Boolean):Stream[TreeLoc[A]] = t.cojoin.tree.flatten.filter(p)
    def replace[A](tree:Tree[A],at:TreeLoc[A]):TreeLoc[A] = TreeLoc(tree, at.lefts,at.rights, at.parents)
  }

  def printTree(node: Tree[Code]): Unit = node.draw foreach println

  object GraphUtil {
    import Tree._

    var mapIRExp2NewIRExp = HashMap.empty[IR.Exp[Any], newIR.Exp[Any]]
    var mapCode2NewIRExp = HashMap.empty[Tree[Code], newIR.Exp[Any]]
    var subtrees = HashMap.empty[Int, Tree[Code]]

    //  A topological sort of the tree. Leafs get sorted out.
    def getSchedule(root: Tree[Code]) = {
      def deps (node: Tree[Code]) : List[Tree[Code]] = node match {
        case Tree.Node(CSX(loc), _) => deps(subtrees(loc))
        case Tree.Node(_, children) => children.filter(t => {
          t match {
            case Tree.Node(CSX(loc),_) => !(subtrees(loc).subForest.isEmpty)
            case _ => !t.subForest.isEmpty
          }
        }).toList
        case _ => List.empty[Tree[Code]]
      }
      val xx = scala.virtualization.lms.util.GraphUtil.stronglyConnectedComponents[Tree[Code]](List(root), t => deps(t))
      xx.flatten.reverse
    }

    def isDefinedCode(code: Tree[Code]) = mapCode2NewIRExp.contains(code)
    def isDefined(expIR: IR.Exp[Any]) = mapIRExp2NewIRExp.contains(expIR)
    def setExpIR2ExpNewIR(expIR: IR.Exp[_], expNewIR: newIR.Exp[_]) = mapIRExp2NewIRExp += (expIR -> expNewIR)
    def setCode2ExpNewIR(code: Tree[Code], expNewIR: newIR.Exp[_]) = mapCode2NewIRExp += (code -> expNewIR)
    def getCode2NewIRExp(code: Tree[Code]): newIR.Exp[Any] = mapCode2NewIRExp(code)
    def getNewIRExp[T](expIR: IR.Exp[T]): newIR.Exp[T] = {
      if ( !mapIRExp2NewIRExp.contains(expIR) ) {
        val expNewIR = (expIR match {
          case s:IR.Sym[_] => newIR.fresh(s.tp)
          case _ => throw new IllegalArgumentException ()
        })
        mapIRExp2NewIRExp += (expIR -> expNewIR)
      }
      mapIRExp2NewIRExp(expIR).asInstanceOf[newIR.Exp[T]]
    }

    def tree2Instructions(root:Tree[Code]): newIR.Exp[Any] = {
      new_inputs = inputs.map (_ => null)
      // println("printing the converted tree")
      // printTree(root)

      def Argument2Object(in : Tree[Code]): Object = {
        in match {
          case Node(CSX(loc),_) => Argument2Object(subtrees(loc))
          case Node(SourceContextCode(pos),_) => pos
          case Node(NumericCode(aev),_) => aev
          case Node(ManifestCode(mev),_) => mev
          case Node(IntegerConstant(i,true),_) => newIR.Const(i)
          case Node(IntegerConstant(i,false),_) => i:java.lang.Integer
          case Node(VariableX(sy@IR.Sym(nr)), _) => {
            val s = getNewIRExp(IR.Sym(nr))
            inputs.indexWhere (input => (input.getRep.asInstanceOf[IR.Sym[IR.Vector]].id == nr) ) match {
              case -1  =>  // do nothing
              case idx => {
                val vec = IR.getDSLVector(IR.Sym(nr))
                new_inputs = new_inputs.updated(idx, new newIR.Vector (s, vec.size))
              }
            }
            s
          }
          case Node(CodeBlock, b) => newIR.reifyEffects(obtainArguments(b)(0).asInstanceOf[newIR.Exp[Any]])
          case tree@Node(n@Instruction(instr), params) => n.sym match {
            case None if isDefinedCode(tree) => getCode2NewIRExp(tree)
            case Some(s:IR.Sym[_]) if isDefined(s) => getNewIRExp(s)
            case None => Reflection.invoke_DSL_infix(instr, obtainArguments(params), false)
          }
        }
      }
      def obtainArguments(params: Stream[Tree[Code]]): List[Object] = params.map ( x => Argument2Object(x)).toList

      def inSchedule(in: Tree[Code]): Unit = {
        in match {
          case Node(CSX(loc),params) => inSchedule(subtrees(loc))
          case tree@Node(inst@Instruction(op), params) => inst.sym match {
            case Some(s)  => if (!isDefined(s)) {
              val result = Reflection.invoke_DSL_infix(op, obtainArguments(params))
              setExpIR2ExpNewIR(s, result.asInstanceOf[newIR.Exp[_]])
            }
            case None if !isDefinedCode(tree) => {
              val result = Reflection.invoke_DSL_infix(op, obtainArguments(params))
              setCode2ExpNewIR(tree, result.asInstanceOf[newIR.Exp[_]])
            }
          }
          case _ => // do nothing        }
      }
      }
      getSchedule(root).foreach ( treeNode => inSchedule(treeNode))

      // Make sure that the root of the tree becomes the new output
      root match {
        case tree@Node(inst@Instruction(op), params) => inst.sym match {
          case Some(s) if (isDefined(s))   => getNewIRExp(s)
          case None if isDefinedCode(tree) => getCode2NewIRExp(tree)
        }
      }
    }

    def tree2dotDigraph(t:Tree[Code], file:String) = {
      import java.io.PrintWriter
      import Tree.Node

      def from(n:Int):Stream[Int] = n #:: from(n+1)
      def alphabet = from(97) map {_.toChar} take 26

      //A unique id generator, needs to contain state
      var id = from(0)
      def uniqueId() = id match { case i#::rest => id = rest;i }

      val treeWithUniqueLabel = t.map{label => (label,uniqueId)}

      def getParameters(children:Stream[Tree[(Code,Int)]]):Stream[String] =
        (children zip alphabet) map {
          case (Node((IntegerConstant(i,true),_),_),_) => "Const("+i+")"
          case (Node((IntegerConstant(i,false),_),_),_) => i.toString
          case (Node((BooleanConstant(b),_),_),_) => b.toString
          case (_,a) 					        => a.toString
        }

      def createDOTlabel(n:Tree[(Code,Int)]) = n match {
        case Node((CodeBlock,_),_) => "Block(a)"
        case Node((ManifestCode(mev),_),_) => "Manifest("+mev+")"
        case Node((SourceContextCode(_),_),_) => "SourceContext(...)"
        case Node((NumericCode(aev),_),_) => aev.getClass.getName.split("\\.").last
        case Node((CSX(loc),_),_) => subtrees(loc).rootLabel//loc.tree.rootLabel
        case Node((label,_),Stream.Empty) => label
        case Node((c@Instruction(label),_),children) => c.sym match {
          case None => "Const("+label+"("+(getParameters(children) mkString ", ")+")"+")"
          case Some(IR.Sym(nr)) => " Sym("+nr+") = " + label+"("+(getParameters(children) mkString ", ")+") "
        }
      }

      val dotDigraph = ("digraph G { \n splines=false;\n" +
        (ExtendedTree(treeWithUniqueLabel).treeLevels.flatMap{ _ map {
          case Node((_:Constant,_),_) => ""
          case tree@Node((n:Code,id:Int),children) => "\""+id+"\" [\n" +
            "label=\""+createDOTlabel(tree)+"\"\n" +
            "shape=box\n]\n" +
            (children zip alphabet).map{
              case (Node((_:Constant,_),_),_) => ""
              case (iTree@Node((c,cId),_),a) => "\""+id+"\" -> \""+cId+"\" [ label=\" "+a+"\" ];"
            }.mkString("\n")
          case _ => ""
        }}).mkString("\n") + "}\n");

      //val dotDigraph =""
      Some(new PrintWriter(file, "UTF-8")).foreach{p => p.write(dotDigraph); p.close}
    }
  }
}
