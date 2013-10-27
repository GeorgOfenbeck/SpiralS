/**
 *  SpiralS - ETH Zurich
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


package ch.ethz.spirals.util

import scala.virtualization.lms.common.ForwardTransformer

trait SubstitutionTransformer extends ForwardTransformer {

  import IR._
  val transformer = this.asInstanceOf[SubstitutionTransformer.this.IR.Transformer]

  def register(from: Exp[Any], to: Exp[Any]) = {
    this.subst += (from -> to)
  }

  override def transformStm(stm: Stm) = stm match {
    case TP(_, e) => mirror(e, transformer)
    case _ => super.transformStm(stm)
  }
}
