/**
 *  SpiralS - ETH Zurich
 *  Copyright (C) 2013  Alen Stojanov  (astojanov@inf.ethz.ch)
 *                      Georg Ofenbeck (ofenbeck@inf.ethz.ch)
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

import scala.virtualization.lms.common.{BaseExp, Base}
import scala.reflect.SourceContext
import scala.virtualization.lms.internal.GenericCodegen

trait Trigonometry extends Base {
  def sin_pi(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def cos_pi(x: Rep[Double], y: Rep[Double]): Rep[Double]
}

trait TrigonometryExp extends Trigonometry with BaseExp {
  case class SinPI(x: Exp[Double], y: Exp[Double]) extends Def[Double]
  case class CosPI(x: Exp[Double], y: Exp[Double]) extends Def[Double]

  def sin_pi(x: Exp[Double],y : Exp[Double]) = SinPI(x,y)
  def cos_pi(x: Exp[Double],y : Exp[Double]) = CosPI(x,y)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@SinPI(l,r) => {
      val (lhs, rhs) = (f(l), f(r))
      sin_pi(lhs.asInstanceOf[Exp[Double]], rhs.asInstanceOf[Exp[Double]]).asInstanceOf[Exp[A]]
    }
    case e@CosPI(l,r) => {
      val (lhs, rhs) = (f(l), f(r))
      cos_pi(lhs.asInstanceOf[Exp[Double]], rhs.asInstanceOf[Exp[Double]]).asInstanceOf[Exp[A]]
    }
    case _ => super.mirror(e,f)
  }
}

trait TrigonometryOps extends GenericCodegen {

  val IR: TrigonometryExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SinPI(Const(a),Const(b)) => emitValDef(sym, Math.sin(a.asInstanceOf[Double] / b.asInstanceOf[Double] * Math.PI).toString)
    case CosPI(Const(a),Const(b)) => emitValDef(sym, Math.cos(a.asInstanceOf[Double] / b.asInstanceOf[Double] * Math.PI).toString)
    case _ => super.emitNode(sym, rhs)
  }
}

