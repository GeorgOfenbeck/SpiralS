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

import ch.ethz.spirals.dsls.{SigmaSPL_DSL => SG}
import ch.ethz.spirals.db.DB
import scalaz.{Tree, TreeLoc, Foldable}
import scala.collection.immutable.HashMap

trait PatternLibrary extends GraphLibrary
{
  class NoSuchParameterFound(msg: String) extends Exception (msg)

  class Pattern {
    var name:Option[String] = None;
    var guard:Option[Guard] = None;

    def toLeaf():Tree[Pattern] = Tree[Pattern](this);
  }
  case class Terminal(ref:Option[String]) extends Pattern {
    name = ref
    override def toString() = "T\""+name+"\""
  }

  case class PatternGetArgument(obj: String, param: String) extends Pattern
  case class Star(ref:String) extends Pattern
  case class IConstant(c:Int) extends Pattern
  case class BConstant(c:Boolean) extends Pattern
  case class PatternDiv(lhsName:String,rhsName:String) extends Pattern //PatternDiv means that it is resolved at construction
  case class InstructionPattern(param: AnyRef) extends Pattern
  object MultiCompose extends Pattern

  abstract class Guard
  case class FunGuard(field:String, fun:Int => Boolean) extends Guard {
    def check(node:AnyRef) = try {
      val fieldObj = node.getClass.getDeclaredField(field)
      fieldObj.setAccessible(true)
      val value = fieldObj.get(node) match { case i:Integer => i.intValue }
      !fun(value)
    } catch {
      case e:NoSuchFieldException => false
    }
  }
  //No logic in here, as this check has to be formulated at a global scope
  case class EqualGuard(a:String, b:String) extends Guard


  object PatternUtil {
    import scala.collection.JavaConversions._
    import scala.reflect.SourceContext
    implicit val spos = implicitly[SourceContext]
    /*def fun[T <: Any](value:T)(implicit m:Manifest[T], n:Numeric[T]) = (m,n)
    def getNumeric[T](m: Manifest[T]): Numeric[_] = {
      println("#"+fun(m.newArray(1)(0)))
      m.toString() match {
        case "Int" => implicitly[Numeric[Int]]
        case "Float" => implicitly[Numeric[Float]]
      }
    }*/
    def getNumeric[T](m: Manifest[T]): Numeric[_] = {
      m.newArray(1)(0) match {
        case _:Int => implicitly[Numeric[Int]]
        case _:Float => implicitly[Numeric[Float]]
      }
    }

    //TODO move this to transformer
    //Create a graph with the replacement-pattern
    def pattern2codetree(pattern:Tree[Pattern], terminals:Map[String,Tree[Code]]):Tree[Code] = {

      //TODO very not nice hack. Make this nicer
      def findManifestCodeTree(node:Tree[Code]):Manifest[_] = {
        node.flatten foreach {
          case ManifestCode(mani) => /*println("Find manifest in ManifestCode "+mani);*/if(mani.toString != "Nothing") return mani.asInstanceOf[Manifest[_]]
          case c:Code => c.sym match {
            case Some(s) => /*println("Find manifest in "+s+" "+c+" "+s.tp);*/if(s.tp.toString != "Nothing") return s.tp
            case None =>
          }
          case _ =>
        }
        null
      }

      def findManifest(node:Stream[Tree[Pattern]]):Manifest[_] = {
        node foreach {
          case Tree.Node(Terminal(Some(ref)),children) => terminals(ref) match {
            case Tree.Node(IntegerConstant(i,_),_) => return manifest[Int]
            case codetree@Tree.Node(code,_) => code.sym match {
              case Some(s) => /*println("Find manifest in "+s+" "+code+" "+s.tp);*/if(s.tp.toString != "Nothing") return s.tp else {
                val mani = findManifestCodeTree(codetree)
                if(mani != null) return mani
              }

              case None => findManifest(children)
            }
          }
          case Tree.Node(InstructionPattern(_), children) => findManifest(children)
          case _ =>
        }
        null
      }
      //END HACK

      def getChildren(op:AnyRef, s:Stream[Tree[Pattern]]) =
        (s zip getParamList(op)).map{
          case (Tree.Node(IConstant(i),_),"int")	=> IntegerConstant(i,false).toLeaf
          case (patChild,"Block")					=> Tree.node(CodeBlock, Stream(patChild.foldNode(mapper)))
          case (patChild,_) 						=> patChild.foldNode(mapper)
        }

      def numericOperationCodeConstruction(op:AnyRef, s:Stream[Tree[Pattern]]) = {
        assert(s.length == 2, "a pattern for a numeric operation should always have two children")
        var children = getChildren(op, s)
        val mani = findManifest(s)
        val numi = getNumeric(mani)
        children = children :+ NumericCode(numi).toLeaf
        children = children :+ ManifestCode(mani).toLeaf
        children = children :+ SourceContextCode(spos).toLeaf

        Tree.node(Instruction(op).setSymAnnotation(IR.fresh[Int]),children)
      }


      def toInt(n:String) = terminals(n) match { case Tree.Node(IntegerConstant(i,_),_) => i}

      def getCompanionObjectFromNode(node: Tree[Code], param: String): AnyRef = node match {
        case Tree.Node(Instruction(instr), _) => instr
        case _ => throw new NoSuchParameterFound(param)
      }

      def getChildAtPos(node: Tree[Code], idx: Int) = node match {
          case Tree.Node(_, children) => children(idx)
      }

      def mapper:(Pattern => (Stream[Tree[Pattern]] => Tree[Code])) = (pat:Pattern) => pat match {
        case PatternGetArgument(obj, param)	 => ((_) => {
          val cObj = getCompanionObjectFromNode(terminals(obj), param)
          val applyMethod = cObj.getClass.getMethods.filter(m => {
            m.getName == "apply" && m.getReturnType.getSimpleName == "Object"
          })(0)
          val arity = applyMethod.getParameterTypes().size
          val nulls = (for(i <- 0 until arity) yield null).toList.asInstanceOf[List[Object]]
          val instance: AnyRef = applyMethod.invoke(cObj, nulls:_*)
          var counter: Int = 0
          var idx = -1
          instance.getClass.getDeclaredFields.map(field => {
            if ( field.getName == param ) idx = counter
            counter += 1
          })
          if ( idx == -1 ) throw new NoSuchParameterFound(param)
          getChildAtPos(terminals(obj), idx)
        })
        case Star(ref)			     => ((_) => terminals(ref))
        case Terminal(Some(ref)) => ((_) => terminals(ref))
        case IConstant(i)        => ((_) => IntegerConstant(i).toLeaf)
        case BConstant(b)        => ((_) => BooleanConstant(b).toLeaf)
        case PatternDiv(lhs,rhs) => ((_) => IntegerConstant(toInt(lhs)/toInt(rhs)).toLeaf)
        //TODO reintegrate that later
        /*case MultiCompose 		 =>
                          //does fold and right afterwards flatten a BC->BC structure to BC
                          ((s:Stream[Tree[Pattern]]) =>
                            Tree.node(BigCompose(), s.map{ case a => a.foldNode(mapper)}.map{
                              case x@Tree.Node(_:BigCompose,c) 	=> c
                              case x 								=> Stream(x)
                            }.flatten)
                          )
        */
        case InstructionPattern(op@IR.NumericTimes) 	=> numericOperationCodeConstruction(op,_)
        case InstructionPattern(op@IR.NumericDivide) 	=> numericOperationCodeConstruction(op,_)
        case InstructionPattern(op@IR.NumericPlus) 		=> numericOperationCodeConstruction(op,_)
        case InstructionPattern(op@IR.NumericMinus) 	=> numericOperationCodeConstruction(op,_)
        case InstructionPattern(op) => 	((s:Stream[Tree[Pattern]]) => {
          var children = getChildren(op, s)
          Tree.node(Instruction(op).setSymAnnotation(IR.fresh[Int]),children)
        })
      }

      pattern.foldNode(mapper)
    }

    def getParamList(param:AnyRef) = {val params = param.getClass.getMethods.toList filter{m => m.getName == "apply"  && m.getModifiers == 1} flatMap {m => m.getParameterTypes.toList map{_.getSimpleName} filter {_!="SourceContext"}};  if(param.toString == "NumericDivide"||param.toString == "NumericTimes"||param.toString == "NumericPlus") params.take(2) else params}

    def check(t:Tree[Pattern]) = t match {
      case Tree.Node(InstructionPattern(param),more) =>
        val paramClassNames = PatternUtil.getParamList(param)
        val className = param.toString

        if(paramClassNames.length != more.length) throw new Error("wrong number of arguments in pattern ("+className+"), has "+more.length + " paramesters but "+paramClassNames.length+" expected")
        val parameterCheckTriple = (paramClassNames, more map {_.rootLabel.getClass.getSimpleName.split("\\$")(0)}, paramClassNames zip (more map {_.rootLabel.getClass.getSimpleName.split("\\$")(0)}) map {
          case (_,"Terminal") => true
          case (_,"InstructionPattern") => true
          case (_,"MultiCompose") => true
          //TODO int and boolean are not 	interconvertible
          case ("int", _) => true
          case ("boolean", _) => true
          case _ => false
        }).zipped.toList
        if(parameterCheckTriple.exists{ !_._3 }) throw new Error("A parameter of the pattern ("+className+") is wrong! Namely "+parameterCheckTriple.filter{ !_._3 }.map{case(classParam,patternType,_) => classParam+" can not be represented with pattern type "+patternType}.mkString(", ")  )
        true
      case _ => true
    }
  }

  //shorter representation
  object PatternSimplificator {
    type PatternTree = Tree[Pattern]
    val ? = Terminal(None).toLeaf;
    val * = Star;
    private def conv(n:Any):PatternTree = n match {
      case str:String 	=> Terminal(Some(str)).toLeaf
      case p:PatternTree 	=> p
      case i:Int 			=> IConstant(i).toLeaf
      case b:Boolean 		=> BConstant(b).toLeaf
      case p:Pattern 		=> p.toLeaf
    }
    case class <(subpatterns:Any*) {
      def >():PatternTree = {
        val pattern = subpatterns.toList match {
          //case (str1:String)::str2::Nil 	=> Tree.node(comp, Stream(str1,str2) map conv)
          case (p:Tree[Pattern])::Nil  	=> p
          //case (p:Tree[Pattern])::w::Nil 	=> Tree.node(comp, Stream(p, conv(w)))
          case (symbol:AnyRef)::Nil 		=>
            val params = PatternUtil.getParamList(symbol)
            Tree.node(InstructionPattern(symbol), Stream.fill(params.length)(Terminal(None).toLeaf))
          case (symbol:AnyRef)::xs 		=> Tree.node(InstructionPattern(symbol), (xs.toStream map conv))
        }
        if( DB.getConf.spiralsconfig.develop) PatternUtil.check(pattern)
        pattern
      }

      def >(name:String):PatternTree = {
        val pat = >()
        pat.rootLabel.name = Some(name)
        pat
      }
      def >(guard:Guard, name:String):PatternTree = {
        val pat = >(name)
        pat.rootLabel.guard = Some(guard)
        pat
      }

    }
    case class <<(ns:Any*) {
      def >>():PatternTree = {
        val pattern = Tree.node(MultiCompose, (ns map conv).toStream)
        if(DB.getConf.spiralsconfig.develop) PatternUtil.check(pattern)
        pattern
      }
    }
  }
  object PredefinedSymbol {
    import PatternSimplificator._

    /* object sigma {
       def apply(ns:Any*) = {
         new {
           def apply(name:String):PatternTree = {
             val loop = InstructionPattern(IR.sigma)
             loop.name = Some(name)
             val pattern = Tree.node(loop,
               Stream(	Terminal(Some("__"+name+"_from")).toLeaf,
                 Terminal(Some("__"+name+"_to")).toLeaf,
                 Terminal(Some("__"+name+"_loopvar")).toLeaf,
                 (<<(ns:_*)>>)
               )
             )
             if(DB.getConf.spiralsconfig.develop) { PatternUtil.check(pattern) }
             pattern
           }
         }
       }
     }

     val Σ = sigma;*/

    //val S = <(Scatter)>("S")
    val X = *("X")
    //val G = <(Gather)>("G")
    val LHS = *("__lhs")
    val RHS = *("__rhs")

    case class <<*(ns:Any*) {
      def *>>():PatternTree = {
        val l = LHS :: ns.toList ::: List(RHS)
        (<<(l:_*)>>)
      }
    }




  }
}
