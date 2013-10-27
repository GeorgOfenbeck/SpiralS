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

package ch.ethz.spirals.dsls

import ch.ethz.spirals.rewrites._
import ch.ethz.spirals.util.{CCompileTransformed, DSLUtils, CCompile}
import ch.ethz.spirals.dsls.cir._

import scala.virtualization.lms.common._

import java.io.PrintWriter
import ch.ethz.spirals.datatypes.{DataTypeFactories}

abstract class CIR_DSL extends GlobalArrayOpsExp with ForLoopFatExp with PrimitiveOpsExp with CIRNumericExp
    with LiftNumeric with LoopsFatExp with IfThenElseFatExp with TrigonometryExp with CommentExp with CCompileTransformed
    with VariablesExp with DataTypeFactories with DSLUtils { self =>

  val codegen : CIRCodegen {
    val IR: self.type
  }

  object CIR_Optimizer extends CIR_Optimizer { val IR: self.type = self }
  val listOfOptimizers = List(CIR_Optimizer)

  def compileOptimized[B](f: List[Exp[Any]] => Exp[B])(implicit mList: List[Manifest[Any]], mB: Manifest[B]) = {
    compileTransformed[B](f, listOfOptimizers)
  }

  def compileOptimized[A,B](f: Exp[A] => Exp[B])(implicit mA: Manifest[A], mB: Manifest[B]) = {
    val func: (List[Exp[Any]] => Exp[B]) = (in: List[Exp[Any]]) => f(in(0).asInstanceOf[Exp[A]])
    implicit val mList = List(mA).asInstanceOf[List[Manifest[Any]]]
    compileTransformed[B](func, listOfOptimizers)
  }

}


















