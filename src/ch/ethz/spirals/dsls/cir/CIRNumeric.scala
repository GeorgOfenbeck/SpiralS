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

package ch.ethz.spirals.dsls.cir

import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import scala.virtualization.lms.internal._

trait CIRNumeric extends NumericOpsExpOpt with PrimitiveOpsExpOpt {
  def infix_mod(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def infix_pow(x: Rep[Int], y: Rep[Int]): Rep[Int]
}

trait CIRNumericExp extends CIRNumeric {

  case class Mod(x: Exp[Int], y: Exp[Int]) extends Def[Int]

  def infix_mod(lhs: Exp[Int], rhs: Exp[Int]) =  (lhs,rhs) match {
    case (Const(x:Int), Const(y:Int)) => Const[Int](x % y)
    case _ => Mod(lhs, rhs)
  }


  case class Pow(x: Exp[Int], y: Exp[Int]) extends Def[Int]

  def infix_pow(lhs: Exp[Int], rhs: Exp[Int]) =  (lhs,rhs) match {
    case (Const(x:Int), Const(y:Int)) => Const[Int](scala.math.pow(x,y).toInt)
    case _ => Pow(lhs, rhs)
  }


  override def numeric_divide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    case (Const(x:Int), Const(y:Int)) => Const[Int](x / y).asInstanceOf[Exp[T]]
    case _ => super.numeric_divide(lhs,rhs)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@Pow(l,r) => {
      val (lhs, rhs) = (f(l), f(r))
      infix_pow(lhs.asInstanceOf[Exp[Int]], rhs.asInstanceOf[Exp[Int]]).asInstanceOf[Exp[A]]
    }
    case e@Mod(l,r) => {
      val (lhs, rhs) = (f(l), f(r))
      infix_mod(lhs.asInstanceOf[Exp[Int]], rhs.asInstanceOf[Exp[Int]]).asInstanceOf[Exp[A]]
    }
    case _ => super.mirror(e,f)
  }
}

trait CIRNumericOps extends CLikeGenNumericOps {
  val IR: CIRNumericExp
  import IR._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Mod(a, b) => emitValDef(sym, quote(a) + " % " + quote(b))
    case Pow(a, b) => emitValDef(sym, "(int) pow((double)" + quote(a) + "," + quote(b) + ")" )
    case _ => super.emitNode(sym, rhs)
  }
}