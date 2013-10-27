/**
 *     _______  _______  ___   __      ____     Automatic
 *    / __/ _ \/  _/ _ \/ _ | / /     / __/     * Implementation
 *   _\ \/ ___// // , _/ __ |/ /__   _\ \       * Optimization
 *  /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation
 *                                              of DSP Algorithms
 *  https://bitbucket.org/GeorgOfenbeck/spirals
 *  SpiralS 0.1 Prototype - ETH Zurich
 *  Copyright (C) 2013  Alen Stojanov  (astojanov@inf.ethz.ch)
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

import scala.virtualization.lms.internal._
import java.io.{StringWriter, PrintWriter}
import virtualization.lms.common._
import scala.Some
import collection.mutable
import scala.reflect.SourceContext

abstract class CodeTransformer extends BlockTraversal {

  val IR: Expressions
  import IR._

  def transform[B](block: (List[Sym[Any]], Block[B])) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) : (List[Sym[Any]], Block[B])
}

import ch.ethz.spirals.dsls._

trait CIR_Optimizer extends CodeTransformer with GraphTraversal with ScalaGenArrayOps with ScalaGenNumericOps {  self =>

  val IR : CIR_DSL
  import IR._

  implicit val spos = implicitly[SourceContext]

  def funcToBlock[B](f: List[IR.Exp[Any]] => IR.Exp[B]) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) = {
    val symList = mList.map(m => fresh(m)).asInstanceOf[List[Sym[Any]]]
    (symList, reifyBlock[B](f(symList)))
  }

  def isZero[T](v: T, e: DefMN[T]) = e.aev.compare(e.aev.plus(v, e.aev.zero), e.aev.zero) == 0
  def isOne [T](v: T, e: DefMN[T]) = e.aev.compare(v, e.aev.one) == 0


  class CIRTransformer extends ForwardTransformer {

    val IR:self.IR.type = self.IR


    protected var negs = Set.empty[Exp[Any]]

    protected def setNeg(sym: Sym[Any], negative: Boolean = true) = negative match {
      case true => negs = negs + sym
      case _ =>
    }

    protected def resetCIRTransformer() = {
      negs = Set.empty[Exp[Any]]
    }

    protected def isNeg[T](exp: Exp[T], e: DefMN[T] = null): Boolean = exp match {
      case s@Sym(_) => negs.contains(s)
      case Const(v) if (e != null) => (e.aev.compare(e.aev.plus(v, e.aev.zero), e.aev.zero) == -1)
      case _ => false
    }

    protected def getSymSubst(s1: Exp[Any], s2: Exp[Any]): (Exp[Any], Exp[Any]) = {
      (subst.getOrElse(s1, s1), subst.getOrElse(s2, s2))
    }

    protected def findDefinitionAndApply[T](x: Exp[Any], f: Stm => Option[T]): Option[T] = {
      if ( x.isInstanceOf[Sym[_]] ) {
        findDefinition(x.asInstanceOf[Sym[Any]]) match {
          case Some(stm) => f(stm)
          case None => None
        }
      } else None
    }

    protected def makeTimes[T](s:Sym[T], e:DefMN[T], lhs:Exp[T], rhs:Exp[T], lhs_neg: Boolean, rhs_neg: Boolean): Option[Exp[T]] = {
      implicit val (aev, mev) = (e.aev, e.mev)
      (lhs_neg, rhs_neg) match {
        case (true, false) => setNeg(s); Some(numeric_times(lhs, rhs))
        case (false, true) => setNeg(s); Some(numeric_times(lhs, rhs))
        case (true, true) => Some(numeric_times(lhs, rhs))
        case (false, false) => None
      }
    }

    protected def makeDivide[T](s:Sym[T], e:DefMN[T], lhs:Exp[T], rhs:Exp[T], lhs_neg: Boolean, rhs_neg: Boolean): Option[Exp[T]] = {
      implicit val (aev, mev) = (e.aev, e.mev)
      (lhs_neg, rhs_neg) match {
        case (true, false) => setNeg(s); Some(numeric_divide(lhs, rhs))
        case (false, true) => setNeg(s); Some(numeric_divide(lhs, rhs))
        case (true, true) => Some(numeric_divide(lhs, rhs))
        case (false, false) => None
      }
    }

    protected def makeMinus[T](s:Sym[T], e:DefMN[T], lhs:Exp[T], rhs:Exp[T], lhs_neg: Boolean, rhs_neg: Boolean): Option[Exp[T]] = {
      implicit val (aev, mev) = (e.aev, e.mev)
      (lhs_neg, rhs_neg) match {
        case (true, false) => setNeg(s); Some(numeric_plus(lhs, rhs))
        case (false, true)  => Some(numeric_plus (lhs, rhs))
        case (true, true)   => Some(numeric_minus(rhs, lhs))
        case (false, false) => None
      }
    }

    protected def makePlus[T](s:Sym[T], e:DefMN[T], lhs:Exp[T], rhs:Exp[T], lhs_neg: Boolean, rhs_neg: Boolean): Option[Exp[T]] = {
      implicit val (aev, mev) = (e.aev, e.mev)
      (lhs_neg, rhs_neg) match {
        case (true, false) => Some(numeric_minus(rhs, lhs))
        case (false, true) => Some(numeric_minus(lhs, rhs))
        case (true, true) => setNeg(s); Some(numeric_plus(lhs, rhs))
        case (false, false) => None
      }
    }

    override def transformStm(stm: Stm): Exp[Any] = stm match {
      case TP(s, e@NumericTimes(a, b)) => makeTimes(s, e, this(a), this(b), isNeg(a), isNeg(b)) match {
          case Some(exp) => exp
          case None => super.transformStm(stm)
      }
      case TP(s, e@NumericDivide(a, b)) => makeDivide(s, e, this(a), this(b), isNeg(a), isNeg(b)) match {
          case Some(exp) => exp
          case None => super.transformStm(stm)
      }
      case TP(s, e@NumericPlus(a, b)) => makePlus(s, e, this(a), this(b), isNeg(a), isNeg(b)) match {
          case Some(exp) => exp
          case None => super.transformStm(stm)
      }
      case TP(s, e@NumericMinus(a, b)) => makeMinus(s, e, this(a), this(b), isNeg(a), isNeg(b)) match {
          case Some(exp) => exp
          case None => super.transformStm(stm)
      }
      case TP(s, SinPI(_, _)) => super.transformStm(stm)
      case TP(s, CosPI(_, _)) => super.transformStm(stm)
      case TP(s, ArrayApply(_, idx)) => {
        assert(!isNeg(idx), "Array can not be indexed with negative index: " + s.toString)
        super.transformStm(stm)
      }
      case TP(s, Reflect(ArrayUpdate(sym, c, s1), summary, deps) ) => {
        if (isNeg(s1)) {
          implicit val aev = findDefinitionAndApply(s1, sstm => infix_rhs(sstm) match {
            case e: DefMN[_] => Some(e.asInstanceOf[DefMN[Any]].aev)
            case _ => throw new Exception ("Array update can not be negative: " + s)
          }).head
          array_update(this(sym), this(c), numeric_minus(aev.zero, this(s1))(aev, this(s1).tp, mpos(s.pos)))
        } else {
          super.transformStm(stm)
        }
      }
      case TP(s, Reflect(ArrayApply(_, idx), _, _)) => {
        assert(!isNeg(idx), "Array can not be indexed with negative index: " + s.toString)
        super.transformStm(stm)
      }
      case TP(s, Reflect(ArrayNew(size), _, _)) => {
        assert(!isNeg(size), "Array creation size can not be negative: " + s.toString)
        super.transformStm(stm)
      }
      case TP(s, Reflect(Comment(_), summary, deps)) => super.transformStm(stm)
      case TP(s, Reflect(ForLoop(start, size, iter, blk), summary, deps)) => {
        assert(!isNeg(size), "The size of the loop can not be negative: " + s.toString)
        super.transformStm(stm)
      }
      case TP(s, Reflect(GlobalConstArrayNew(size), _, _)) => super.transformStm(stm)
      case TP(s, Reify(s1, _, _)) => {
        assert(!isNeg(s1), "Symbol in Reify can not be negative: " + s.toString)
        super.transformStm(stm)
      }
      case TP(s, Mod(s1, s2)) => {
        assert(!isNeg(s1) && !isNeg(s2), "Modulo can not have negative components: " + s.toString)
        super.transformStm(stm)
      }

      case _ => super.transformStm(stm)
    }
  }

  def opsCount[B](block:(List[Sym[Any]], Block[B])) : (Int, Int) = {
    val opsTraversal = new NestedBlockTraversal {
      val IR:self.IR.type = self.IR
      var stack: mutable.Stack[(Int, Int)] = null
      def incAdd (v: Int): Unit = {
        val (a, b) = stack.pop()
        stack.push((a+v, b))
      }
      def incMul (v: Int): Unit = {
        val (a, b) = stack.pop()
        stack.push((a, b+v))
      }
      override def traverseStm(stm: Stm): Unit = {
        stm match {
          case TP(_, NumericPlus(_, _))   => incAdd(1)
          case TP(_, NumericMinus(_, _))  => incAdd(1)
          case TP(_, NumericTimes(_, _))  => incMul(1)
          case TP(_, NumericDivide(_, _)) => incMul(1)
          case TP(_, Mod(_, _))           => incMul(1)
          case TP(_, Reflect(ForLoop(_, Const(size), _, _y), _, _)) => {
            stack.push((0,0))
            super.traverseStm(stm)
            val (add, mul) = stack.pop()
            incAdd(size * add)
            incMul(size * mul)
          }
          case _ =>
        }
        super.traverseStm(stm)
      }
      def run[B] (block: Block[B]) : (Int, Int) = {
        stack = new mutable.Stack[(Int, Int)] ()
        stack.push((0,0))
        traverseBlock(block)
        stack.pop()
      }
    }
    opsTraversal.run(block._2)
  }

  def getConstants[B](b:(List[Sym[Any]], Block[B])) : List[Double] = {
    val constTraversal = new NestedBlockTraversal {
      val IR:self.IR.type = self.IR
      var constants = List.empty[Double]
      override def traverseStm(stm: Stm): Unit = {
        stm match {
          case TP(_, e@NumericPlus(Const(x), Const(y)))   => constants = e.aev.toDouble(x) :: e.aev.toDouble(y) :: constants
          case TP(_, e@NumericPlus(Const(x), _))          => constants = e.aev.toDouble(x) :: constants
          case TP(_, e@NumericPlus(_, Const(x)))          => constants = e.aev.toDouble(x) :: constants
          case TP(_, e@NumericMinus(Const(x), Const(y)))  => constants = e.aev.toDouble(x) :: e.aev.toDouble(y) :: constants
          case TP(_, e@NumericMinus(Const(x), _))         => constants = e.aev.toDouble(x) :: constants
          case TP(_, e@NumericMinus(_, Const(x)))         => constants = e.aev.toDouble(x) :: constants
          case TP(_, e@NumericTimes(Const(x), Const(y)))  => constants = e.aev.toDouble(x) :: e.aev.toDouble(y) :: constants
          case TP(_, e@NumericTimes(Const(x), _))         => constants = e.aev.toDouble(x) :: constants
          case TP(_, e@NumericTimes(_, Const(x)))         => constants = e.aev.toDouble(x) :: constants
          case TP(_, e@NumericDivide(Const(x), Const(y))) => constants = e.aev.toDouble(x) :: e.aev.toDouble(y) :: constants
          case TP(_, e@NumericDivide(Const(x), _))        => constants = e.aev.toDouble(x) :: constants
          case TP(_, e@NumericDivide(_, Const(x)))        => constants = e.aev.toDouble(x) :: constants
          case TP(_, e@SinPI(Const(x), Const(y)))         => constants = Math.sin(x / y * Math.PI) :: constants
          case TP(_, e@CosPI(Const(x), Const(y)))         => constants = Math.cos(x / y * Math.PI) :: constants
          case _ =>
        }
        super.traverseStm(stm)
      }
    }
    constTraversal.traverseBlock(b._2)
    constTraversal.constants.distinct.sorted
  }

  val printer = new NestedBlockTraversal {
    val IR:self.IR.type = self.IR
    override def traverseStm(stm: Stm): Unit = stm match {
      case _ => {
        super.traverseStm(stm)
        System.out.println(stm.toString())
      }
    }
  }

  def optimizations[B](block:(List[Sym[Any]],Block[B])) (implicit mA: List[Manifest[Any]], mB: Manifest[B]) : (List[Sym[Any]], Block[B]) = {
    var transBlock = block
    transBlock = replaceTwiddles(transBlock)
    transBlock = elimZeroArithmetics(transBlock)
    transBlock = makeConstantsPositive(transBlock)
    transBlock = elimConstArithmetics(transBlock)
    // transBlock = algebraicTransformations(transBlock)
    // transBlock = pushMultiplicationsForward(transBlock)
    transBlock = CSE_1(transBlock)
    transBlock = CSE_2(transBlock)
    transBlock = CSE_1(transBlock)
    transBlock
  }

  def transform[B](block: (List[Sym[Any]], Block[B])) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) : (List[Sym[Any]], Block[B]) = {
    optimizations(block)
  }

  private def removeDoubleAdds[B](block:(List[Sym[Any]], Block[B])) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) : (List[Sym[Any]], Block[B]) = {
    val transformer = new CIRTransformer {

      private def matchAddPattern(exp: Exp[Any]): Option[Exp[Any]] = findDefinitionAndApply(exp, stm => stm match {
        case TP(s, e@NumericPlus(s1, s2)) => {
          val (lhs, rhs) = getSymSubst (s1, s2)
          lhs.equals(rhs) match {
            case true => Some(lhs)
            case _ => None
          }
        }
        case _ => None
      })

      override def transformStm(stm: Stm): Exp[Any] = stm match {
        case TP(s, e@NumericTimes(c@Const(v), op)) => {
          matchAddPattern(subst.getOrElse(op, op)) match {
            case Some(exp) => numeric_times(exp, Const(e.aev.plus(v, v)))(e.aev, e.mev, mpos(s.pos))
            case _ => super.transformStm(stm)
          }
        }
        case TP(s, e@NumericTimes(op, c@Const(v))) => {
          matchAddPattern(subst.getOrElse(op, op)) match {
            case Some(exp) => numeric_times(exp, Const(e.aev.plus(v, v)))(e.aev, e.mev, mpos(s.pos))
            case _ => super.transformStm(stm)
          }
        }

        case _ => super.transformStm(stm)
      }
    }

    (block._1, transformer.transformBlock(block._2))
  }

  private def CSE_2[B](block:(List[Sym[Any]], Block[B])) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) : (List[Sym[Any]], Block[B]) = {
    val transformer = new CIRTransformer {

      private def matchAddSubPattern(exp: Exp[Any]): Option[(Int, Exp[Any], Exp[Any])] = findDefinitionAndApply(exp, stm => stm match {
        case TP(s, e@NumericPlus(s1, s2)) => {
          val (lhs, rhs) = getSymSubst (s1, s2)
          Some(1, lhs, rhs)
        }
        case TP(s, e@NumericMinus(s1, s2)) => {
          val (lhs, rhs) = getSymSubst (s1, s2)
          Some(-1, lhs, rhs)
        }
        case _ => None
      })

      private def createAddSubExp(sign: Int, s:Sym[Any], e: DefMN[Any], lhs: Exp[Any], rhs: Exp[Any]): Exp[Any] = sign match {
        case 1 => numeric_plus(lhs, rhs)(e.aev, e.mev, mpos(s.pos))
        case _ => numeric_minus(lhs, rhs)(e.aev, e.mev, mpos(s.pos))
      }

      private def matchExpression(sM: Int, s:Sym[Any], e: DefMN[Any], lhs: Exp[Any], rhs: Exp[Any]): Option[Exp[Any]] = {
        (matchAddSubPattern(lhs), matchAddSubPattern(rhs)) match {
          case (Some((sL, a, b)), Some((sR, a2, b2))) if (sL != sR) => {
            if ( a.equals(a2) && b.equals(b2) ) {
              sM match {
                case 1 => Some(createAddSubExp(1, s, e, a, a))
                case _ => {
                  val right = createAddSubExp(1, s, e, b, b)
                  Some(createAddSubExp(sL, s, e, Const(e.aev.zero), right))
                }
              }
            } else if ( a.equals(b2) && b.equals(a2) ) {
              sM * sR match {
                case 1 => Some(createAddSubExp(1, s, e, a, a))
                case _ => {
                  val right = createAddSubExp(1, s, e, b, b)
                  sR * (-1) match {
                    case 1 => Some(right)
                    case _ => Some(createAddSubExp(sR * (-1), s, e, Const(e.aev.zero), right))
                  }
                }
              }
            } else {
              None
            }
          }
          case _ => None
        }
      }

      override def transformStm(stm: Stm): Exp[Any] = stm match {
        case TP(s, e@NumericPlus(s1, s2)) => {
          val (lhs, rhs) = getSymSubst (s1, s2)
          matchExpression(1, s, e, lhs, rhs) match {
            case Some(exp) => exp
            case None => super.transformStm(stm)
          }
        }
        case TP(s, e@NumericMinus(s1, s2)) => {
          val (lhs, rhs) = getSymSubst (s1, s2)
          matchExpression(-1, s, e, lhs, rhs) match {
            case Some(exp) => exp
            case None => super.transformStm(stm)
          }
        }
        case _ => super.transformStm(stm)
      }
    }

    var transBlock = (block._1, transformer.transformBlock(block._2))
    transBlock = makeConstantsPositive(transBlock)
    removeDoubleAdds(transBlock)
  }

  private def CSE_1[B](block:(List[Sym[Any]], Block[B])) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) : (List[Sym[Any]], Block[B]) = {

    val transformer = new CIRTransformer {

      var hasChange = false

      private def performCSE(stm: Stm, lhs: Sym[Any], rhs:Sym[Any]) = stm match {
        case TP(s, e@NumericPlus(_, _)) => {
          findDefinitionByDeps(Set(lhs, rhs)).find ( fstm => fstm match {
            case TP(sNew, NumericPlus(_, _)) => true
            case _ => false
          }) match {
            case Some(fstm) if (!fstm.equals(stm)) => {
              val sNew = infix_lhs(fstm).head
              setNeg(s, isNeg(sNew))
              hasChange = true
              sNew
            }
            case _ => super.transformStm(stm)
          }
        }
        case TP(s, e@NumericMinus(_, _)) => {
          findDefinitionByDeps(Set(lhs, rhs)).find ( fstm => fstm match {
            case TP(sNew, NumericMinus(_, _)) => true
            case _ => false
          }) match {
            case Some(fstm) if(!stm.equals(fstm)) => fstm match {
              case TP(sNew, NumericMinus(t1, t2)) => {
                lhs.equals(t2) && rhs.equals(t1) match {
                  case true => setNeg(s, !isNeg(sNew))
                  case false => setNeg(s, isNeg(sNew))
                }
                hasChange = true
                sNew
              }
              case _ => super.transformStm(stm)
            }
            case _ => super.transformStm(stm)
          }
        }
      }

      override def transformStm(stm: Stm): Exp[Any] = stm match {
        case TP(s, e@NumericPlus(s1, s2)) => {
          val (lhs, rhs) = getSymSubst(s1, s2)
          makePlus(s, e, lhs, rhs, isNeg(s1), isNeg(s2)) match {
            case Some(exp) => exp
            case None => {
              if (lhs.isInstanceOf[Sym[_]] && rhs.isInstanceOf[Sym[_]]) {
                performCSE(stm, lhs.asInstanceOf[Sym[Any]], rhs.asInstanceOf[Sym[Any]])
              } else {
                super.transformStm(stm)
              }
            }
          }
        }
        case TP(s, e@NumericMinus(s1@Sym(_), s2@Sym(_))) => {
          val (lhs, rhs) = getSymSubst(s1, s2)
          makeMinus(s, e, lhs, rhs, isNeg(s1), isNeg(s2)) match {
            case Some(exp) => exp
            case None => {
              if (lhs.isInstanceOf[Sym[_]] && rhs.isInstanceOf[Sym[_]]) {
                performCSE(stm, lhs.asInstanceOf[Sym[Any]], rhs.asInstanceOf[Sym[Any]])
              } else {
                super.transformStm(stm)
              }
            }
          }
        }
        case _ => super.transformStm(stm)
      }

      override def transformBlock[A:Manifest](block: Block[A]): Block[A] = {
        var transBlock = block
        do {
          resetCIRTransformer()
          hasChange = false
          transBlock = super.transformBlock(transBlock)
        } while (hasChange)
        transBlock
      }

    }
    (block._1, transformer.transformBlock(block._2))
  }

  private def pushMultiplicationsForward[B](block:(List[Sym[Any]], Block[B])) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) : (List[Sym[Any]], Block[B]) = {

    val transformer = new CIRTransformer {

      private def matchTimesPattern(exp: Exp[Any]) = findDefinitionAndApply(exp, stm => stm match {
        case TP(s, e@NumericTimes(c@Const(_), op)) => Some((c, op))
        case TP(s, e@NumericTimes(op, c@Const(_))) => Some((c, op))
        case _ => None
      })

      private def matchAddSubPattern(exp: Exp[Any]) = findDefinitionAndApply(exp, stm => stm match {
        case TP(s, e@NumericPlus(s1, s2)) => {
          val (lhs, rhs) = getSymSubst (s1, s2)
          (matchTimesPattern(lhs), matchTimesPattern(rhs)) match {
            case (Some((c1, v1)), Some((c2, v2))) => Some((1, c1, v1, c2, v2))
            case _ => None
          }
        }
        case TP(s, e@NumericMinus(s1, s2)) => {
          val (lhs, rhs) = getSymSubst (s1, s2)
          (matchTimesPattern(lhs), matchTimesPattern(rhs)) match {
            case (Some((c1, v1)), Some((c2, v2))) => Some((-1, c1, v1, c2, v2))
            case _ => None
          }
        }
        case _ => None
      })

      private def createAddSubExp(sign: Int, s:Sym[Any], e: DefMN[Any], lhs: Exp[Any], rhs: Exp[Any]): Exp[Any] = sign match {
        case 1 => numeric_plus(lhs, rhs)(e.aev, e.mev, mpos(s.pos))
        case _ => numeric_minus(lhs, rhs)(e.aev, e.mev, mpos(s.pos))
      }

      private def matchExpression(sM: Int, s:Sym[Any], e: DefMN[Any], s1:Exp[Any], s2:Exp[Any]): Option[Exp[Any]] = {
        var result: Option[Exp[Any]] = None
        if ( s1.isInstanceOf[Sym[_]] && s2.isInstanceOf[Sym[_]]) {
          val (lhs, rhs) = getSymSubst (s1, s2)
          (matchAddSubPattern(lhs), matchAddSubPattern(rhs)) match {
            case (Some((sL, c1, v1, c2, v2)), Some((sR, c3, v3, c4, v4))) => {
              if ( c1.equals(c3) && c2.equals(c4) ) {
                val left  = createAddSubExp(sM          , s, e, v1, v3)
                val right = createAddSubExp(sM * sR * sL, s, e, v2, v4)
                val lM = numeric_times(c1, left )(e.aev, e.mev, mpos(s.pos))
                val rM = numeric_times(c2, right)(e.aev, e.mev, mpos(s.pos))
                result = Some(createAddSubExp(sL, s, e, lM, rM))
              } else if ( c1.equals(c4) && c2.equals(c3) ) {
                val left  = createAddSubExp(sM * sR, s, e, v1, v4)
                val right = createAddSubExp(sM * sL, s, e, v2, v3)
                val lM = numeric_times(c1, left )(e.aev, e.mev, mpos(s.pos))
                val rM = numeric_times(c2, right)(e.aev, e.mev, mpos(s.pos))
                result = Some(createAddSubExp(sL, s, e, lM, rM))
              }
            }
            case _ =>
          }
        }
        result
      }

      override def transformStm(stm: Stm): Exp[Any] = stm match {
        case TP(s, e@NumericPlus(s1, s2)) => {
          matchExpression(1, s, e, s1, s2) match {
            case Some(exp) => exp
            case _ => super.transformStm(stm)
          }
        }
        case TP(s, e@NumericMinus(s1, s2)) => {
          matchExpression(-1, s, e, s1, s2) match {
            case Some(exp) => exp
            case None =>  super.transformStm(stm)
          }
        }

        case _ => super.transformStm(stm)
      }
    }

    (block._1, transformer.transformBlock(block._2))
  }

  private def algebraicTransformations[A, B](block:(Sym[A], Block[B])) (implicit mB: Manifest[B]) : (Sym[A], Block[B]) = {
    val transformer = new CIRTransformer {
      private def getTimesOperands(sym: Exp[Any]): Option[(Const[Any], Exp[Any])] = {
        if ( sym.isInstanceOf[Sym[_]] ) {
          findDefinition(sym.asInstanceOf[Sym[Any]]) match {
            case Some(stm) => stm match {
              case TP(s, NumericTimes(c@Const(_), op)) => Some((c, op))
              case TP(s, NumericTimes(op, c@Const(_))) => Some((c, op))
              case _ => None
            }
            case None => None
          }
        } else None
      }
      override def transformStm(stm: Stm): Exp[Any] = stm match {
        case TP(s, e@NumericPlus(s1, s2)) => {
          val (lhs, rhs) = getSymSubst(s1, s2)
          (getTimesOperands(lhs), getTimesOperands(rhs)) match {
            case (Some((c1, left)), Some((c2, right))) if (c1.equals(c2)) => {
              numeric_times(numeric_plus(left, right)(e.aev, e.mev, mpos(s.pos)), c1)(e.aev, e.mev, mpos(s.pos))
            }
            case _ => super.transformStm(stm)
          }
        }
        case TP(s, e@NumericMinus(s1, s2)) => {
          val (lhs, rhs) = getSymSubst(s1, s2)
          (getTimesOperands(lhs), getTimesOperands(rhs)) match {
            case (Some((c1, left)), Some((c2, right))) if (c1.equals(c2)) => {
              numeric_times(numeric_minus(left, right)(e.aev, e.mev, mpos(s.pos)), c1)(e.aev, e.mev, mpos(s.pos))
            }
            case _ => super.transformStm(stm)
          }
        }
        case _ => super.transformStm(stm)
      }
    }
    (block._1, transformer.transformBlock(block._2))
  }

  def makeConstantsPositive[B](block:(List[Sym[Any]], Block[B])) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) : (List[Sym[Any]], Block[B]) = {
    val transformer = new CIRTransformer {

      private def makePositive[T:Manifest](exp: Exp[T], e: DefMN[T]) : (Boolean, Exp[T]) = exp match {
        case c@Const(v) if (isNeg(c, e)) => (true, Const(e.aev.abs(v)))
        case _ => (false, exp)
      }

      override def transformStm(stm: Stm): Exp[Any] = stm match {
        case TP(s, e@NumericPlus(a, b)) => {
          val (lhs_chg, lhs) = makePositive(this(a), e)
          val (rhs_chg, rhs) = makePositive(this(b), e)
          makePlus(s, e, lhs, rhs, lhs_chg || isNeg(a), rhs_chg || isNeg(b)) match {
            case Some(exp) => exp
            case None => super.transformStm(stm)
          }
        }
        case TP(s, e@NumericMinus(Const(v), b)) if isZero(v, e) => {
          if ( !isNeg(b) ) setNeg(s); this(b)
        }
        case TP(s, e@NumericMinus(a, b)) => {
          val (lhs_chg, lhs) = makePositive(this(a), e)
          val (rhs_chg, rhs) = makePositive(this(b), e)
          makeMinus(s, e, lhs, rhs, lhs_chg || isNeg(a), rhs_chg || isNeg(b)) match {
            case Some(exp) => exp
            case None => super.transformStm(stm)
          }
        }
        case TP(s, e@NumericTimes(a, b)) => {
          val (lhs_chg, lhs) = makePositive(this(a), e)
          val (rhs_chg, rhs) = makePositive(this(b), e)
          makeTimes(s, e, lhs, rhs, lhs_chg || isNeg(a), rhs_chg || isNeg(b)) match {
            case Some(exp) => exp
            case None => super.transformStm(stm)
          }
        }
        case _ => super.transformStm(stm)
      }
    }
    (block._1, transformer.transformBlock(block._2))
  }

  private def elimConstArithmetics[B](block:(List[Sym[Any]], Block[B])) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) : (List[Sym[Any]], Block[B]) = {
    val transformer = new ForwardTransformer {
      val IR:self.IR.type = self.IR
      def matchPattern(d: DefMN[Any], sym: Exp[Any], const: Const[Any]) : Option[Exp[Any]] = {
        implicit val (aev, mev) = (d.aev, d.mev)
        if (sym.isInstanceOf[Sym[_]]) d match {
          case e@NumericPlus(_, _) => findDefinition(sym.asInstanceOf[Sym[Any]]) match {
            case Some(stm) => stm match {
              case TP(_, NumericPlus(s, Const(v)))  => Some(numeric_plus (this(s), Const( e.aev.plus (const.x, v) )))
              case TP(_, NumericPlus(Const(v), s))  => Some(numeric_plus (this(s), Const( e.aev.plus (const.x, v) )))
              case TP(_, NumericMinus(s, Const(v))) => Some(numeric_plus (this(s), Const( e.aev.minus(const.x, v) )))
              case TP(_, NumericMinus(Const(v), s)) => Some(numeric_minus(Const( e.aev.plus (const.x, v) ), this(s)))
              case _ => None
            }
            case None => None
          }
          case e@NumericTimes(_, _) => findDefinition(sym.asInstanceOf[Sym[Any]]) match {
            case Some(stm) => stm match {
              case TP(_, NumericTimes(s, Const(v)))  => Some(numeric_times(this(s), Const( e.aev.times(const.x, v) )))
              case TP(_, NumericTimes(Const(v), s))  => Some(numeric_times(this(s), Const( e.aev.times(const.x, v) )))
              case TP(_, ed@NumericDivide(s, Const(v))) if (ed.isInstanceOf[Fractional[_]]) => {
                implicit val faev = ed.aev.asInstanceOf[Fractional[Any]]
                Some(numeric_times(this(s), Const( faev.div(const.x, v) ))(aev, mev, self.spos))
              }
              case TP(_, ed@NumericDivide(Const(v), s)) if (ed.isInstanceOf[Fractional[_]]) => {
                Some(numeric_divide(Const(ed.aev.times(const.x, v)), this(s))(ed.aev, ed.mev, mpos(s.pos)))
              }
              case _ => None
            }
            case None => None
          }
          case e@NumericMinus(_, Const(_)) => findDefinition(sym.asInstanceOf[Sym[Any]]) match {
            case Some(stm) => stm match {
              case TP(_, NumericPlus(s, Const(v)))  => Some(numeric_plus (this(s), Const( e.aev.minus(v, const.x) )))
              case TP(_, NumericPlus(Const(v), s))  => Some(numeric_plus (this(s), Const( e.aev.minus(v, const.x) )))
              case TP(_, NumericMinus(s, Const(v))) => Some(numeric_minus(this(s), Const( e.aev.plus (v, const.x) )))
              case TP(_, NumericMinus(Const(v), s)) => Some(numeric_minus(Const( e.aev.minus(v, const.x) ), this(s)))
              case _ => None
            }
            case None => None
          }
          case e@NumericMinus(Const(_), _) => findDefinition(sym.asInstanceOf[Sym[Any]]) match {
            case Some(stm) => stm match {
              case TP(_, NumericPlus(s, Const(v)))  => Some(numeric_minus(Const( e.aev.minus(const.x, v) ), this(s)))
              case TP(_, NumericPlus(Const(v), s))  => Some(numeric_minus(Const( e.aev.minus(const.x, v) ), this(s)))
              case TP(_, NumericMinus(s, Const(v))) => Some(numeric_minus(Const( e.aev.plus (const.x, v) ), this(s)))
              case TP(_, NumericMinus(Const(v), s)) => Some(numeric_plus (Const( e.aev.minus(const.x, v) ), this(s)))
              case _ => None
            }
            case None => None
          }
          case e@NumericDivide(_, Const(_)) if (e.isInstanceOf[Fractional[_]]) => findDefinition(sym.asInstanceOf[Sym[Any]]) match {
            case Some(stm) => stm match {
              case TP(_, NumericTimes(s, Const(v)))  => {
                val aev = e.aev.asInstanceOf[Fractional[Any]]
                Some(numeric_divide(this(s), Const( aev.div(v, const.x) ))(aev, mev, self.spos))
              }
              case TP(_, NumericTimes(Const(v), s))  => {
                val aev = e.aev.asInstanceOf[Fractional[Any]]
                Some(numeric_divide(this(s), Const( aev.div(v, const.x) ))(aev, mev, self.spos))
              }
              case TP(_, ed@NumericDivide(s, Const(v))) if (ed.isInstanceOf[Fractional[_]]) => {
                Some(numeric_divide(this(s), Const( e.aev.times(const.x, v) ))(aev, mev, self.spos))
              }
              case TP(_, ed@NumericDivide(Const(v), s)) if (ed.isInstanceOf[Fractional[_]]) => {
                val aev = e.aev.asInstanceOf[Fractional[Any]]
                Some(numeric_divide( Const(aev.div(v, const.x)), this(s))(aev, mev, self.spos))
              }
              case _ => None
            }
            case None => None
          }
          case e@NumericDivide(Const(_), _) if (e.isInstanceOf[Fractional[_]]) => findDefinition(sym.asInstanceOf[Sym[Any]]) match {
            case Some(stm) => stm match {
              case TP(_, NumericTimes(s, Const(v)))  => {
                val aev = e.aev.asInstanceOf[Fractional[Any]]
                Some(numeric_divide(Const(aev.div(const.x, v)), this(s))(aev, mev, self.spos))
              }
              case TP(_, NumericTimes(Const(v), s))  => {
                val aev = e.aev.asInstanceOf[Fractional[Any]]
                Some(numeric_divide(Const(aev.div(const.x, v)), this(s))(aev, mev, self.spos))
              }
              case TP(_, ed@NumericDivide(s, Const(v))) if (ed.isInstanceOf[Fractional[_]]) => {
                Some(numeric_divide(Const(e.aev.times(const.x, v)), this(s))(aev, mev, self.spos))
              }
              case TP(_, ed@NumericDivide(Const(v), s)) if (ed.isInstanceOf[Fractional[_]]) => {
                val aev = e.aev.asInstanceOf[Fractional[Any]]
                Some(numeric_times( Const(aev.div(const.x, v)), this(s))(aev, mev, self.spos))
              }
              case _ => None
            }
            case None => None
          }
          case _ => None
        } else {
          None
        }
      }

      def matchOperands(stm: Stm, e:DefMN[Any], s1: Exp[Any], s2: Exp[Any]) = {
        (this(s1), this(s2)) match {
          case (lhs, Const(v)) => matchPattern(e, lhs, Const(v)) match {
            case Some(exp) => exp
            case None => super.transformStm(stm)
          }
          case (Const(v), rhs) => matchPattern(e, rhs, Const(v)) match {
            case Some(exp) => exp
            case None => super.transformStm(stm)
          }
          case _ => super.transformStm(stm)
        }
      }

      override def transformStm(stm: Stm): Exp[Any] = stm match {
        case TP(s, e@NumericPlus(s1, s2)) => matchOperands(stm, e, s1, s2)
        case TP(s, e@NumericMinus(s1, s2)) => matchOperands(stm, e, s1, s2)
        case TP(s, e@NumericTimes(s1, s2))  => matchOperands(stm, e, s1, s2)
        case TP(s, e@NumericDivide(s1, s2)) => matchOperands(stm, e, s1, s2)
        case _ => super.transformStm(stm)
      }
    }
    (block._1, transformer.transformBlock(block._2))
  }


  def elimZeroArithmetics[B](block:(List[Sym[Any]], Block[B])) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) : (List[Sym[Any]], Block[B]) = {
    val transformer = new ForwardTransformer {
      val IR:self.IR.type = self.IR
      override def transformStm(stm: Stm): Exp[Any] = stm match {
        case TP(s, e@NumericPlus(a, b)) => (this(a), this(b)) match {
          case (Const(v1), Const(v2)) => Const(e.aev.plus(v1, v2))
          case (lhs, Const(v)) if isZero(v, e) => lhs
          case (Const(v), rhs) if isZero(v, e) => rhs
          case _ => super.transformStm(stm)
        }
        case TP(s, e@NumericMinus(a, b)) => (this(a), this(b)) match {
          case (Const(v1), Const(v2)) => Const(e.aev.minus(v1, v2))
          case (lhs, Const(v)) if isZero(v, e) => lhs
          case _ => super.transformStm(stm)
        }
        case TP(s, e@NumericTimes(a, b)) => (this(a), this(b)) match {
          case (Const(v1), Const(v2)) => Const(e.aev.times(v1, v2))
          case (_, Const(v))   if isZero(v, e) => Const(e.aev.zero)
          case (Const(v), _)   if isZero(v, e) => Const(e.aev.zero)
          case (lhs, Const(v)) if isOne(v, e)  => lhs
          case (Const(v), rhs) if isOne(v, e)  => rhs
          case _ => super.transformStm(stm)
        }
        case _ => super.transformStm(stm)
      }
    }
    (block._1, transformer.transformBlock(block._2))
  }


  def replaceTwiddles[B](block:(List[Sym[Any]], Block[B])) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) : (List[Sym[Any]], Block[B]) = {

//    val transformer = new SubstitutionTransformer { val IR: self.IR.type = self.IR }
//    var twiddlesMap = mutable.HashMap.empty[Exp[Any], (List[Any], Manifest[Any])]
//
//    val extractTwiddles = new NestedBlockTraversal {
//      val IR:self.IR.type = self.IR
//      override def traverseStm(stm: Stm): Unit = stm match {
//        case TP(s, e@GlobalConstArrayNew(list)) => {
//          twiddlesMap += (s -> (list, e.m))
//        }
//        case _ => super.traverseStm(stm)
//      }
//    }
//    extractTwiddles.traverseBlock(block._2)
//
//    val generateTwiddlesSubstitutions =  new NestedBlockTraversal {
//      val IR:self.IR.type = self.IR
//      override def traverseStm(stm: Stm): Unit = stm match {
//        case TP(s, ArrayApply(array, Const(idx))) if (twiddlesMap.contains(array)) => {
//          val (list, m) = twiddlesMap.getOrElse(array, null)
//          assert(idx < list.size, "Index out of bound in Twiddles: " + stm)
//          // s.tp.
//          transformer.register(s, Const(list(idx))(m))
//        }
//        case _ => super.traverseStm(stm)
//      }
//    }
//    generateTwiddlesSubstitutions.traverseBlock(block._2)
//
//    twiddlesMap map println
//
//    (block._1, transformer.transformBlock(block._2))
    block
  }


}

