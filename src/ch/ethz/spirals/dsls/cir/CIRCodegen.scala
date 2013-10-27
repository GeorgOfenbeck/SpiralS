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

import ch.ethz.spirals.rewrites.CUnparser
import ch.ethz.spirals.dsls.CIR_DSL

import scala.virtualization.lms.common.{CGenVariables, CGenPrimitiveOps}
import scala.virtualization.lms.util.GraphUtil
import scala.collection.mutable

import java.io.PrintWriter
import ch.ethz.spirals.dsls.instrinsics.ISAGen

trait CIRCodegen extends CUnparser with CGenArrayOpsExt with CGenPrimitiveOps with TrigonometryOps
  with GenComment with CGenForOps with CGenVariables with CIRNumericOps with ISAGen  {

  val IR: CIR_DSL
  import IR._

  def emitOptimizedSource[A, B](f: Exp[A] => Exp[B], className: String, out: PrintWriter)
                               (implicit mA: Manifest[A], mB: Manifest[B]) = {
    val func: (List[Exp[Any]] => Exp[B]) = (in: List[Exp[Any]]) => f(in(0).asInstanceOf[Exp[A]])
    implicit val mList = List(mA).asInstanceOf[List[Manifest[Any]]]
    emitOptimizedSource[B](func, className, out)
  }

  def emitOptimizedSource[B](f: List[Exp[Any]] => Exp[B], className: String, out: PrintWriter)
                            (implicit mList: List[Manifest[Any]], mB: Manifest[B]) = {
    emitTransformedSource[B](f, className, out, IR.listOfOptimizers)
  }

  override def getSchedule(scope: List[Stm])(result: Any, sort: Boolean = true): List[Stm] = {
    val scopeCache = new mutable.HashMap[Sym[Any],Stm]
    for (stm <- scope; s <- stm.lhs)
      scopeCache(s) = stm

    def deps(st: List[Sym[Any]]): List[Stm] = {//st flatMap (scopeCache.get(_).toList)
      // scope.filter(d => (st intersect d.lhs).nonEmpty)
      // scope.filter(d => containsAny(st, d.lhs))
      st flatMap (scopeCache.get(_).toList)
    }

    val xx = GraphUtil.stronglyConnectedComponents[Stm](deps(syms(result)), t => deps(syms(t.rhs)))
    if (sort) xx.foreach { x =>
      if (x.length > 1) {
        printerr("warning: recursive schedule for result " + result + ": " + x)
        (new Exception) printStackTrace
      }
    }
    xx.flatten.reverse

  }

}
