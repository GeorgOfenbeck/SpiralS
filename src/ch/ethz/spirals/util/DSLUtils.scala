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

import scala.virtualization.lms.internal.{Effects, NestedBlockTraversal}
import scala.virtualization.lms.common.{PrimitiveOpsExpOpt, NumericOpsExpOpt, PrimitiveOpsExp, NumericOpsExp}
import ch.ethz.spirals.datatypes.DSLBaseTypes

trait DSLUtils extends DSLBaseTypes with Effects with NumericOpsExpOpt with PrimitiveOpsExpOpt { self =>

  def printBlock[T](block: Block[T]) {
    var indent = 0;
    val printer = new NestedBlockTraversal {
      val IR: self.type = self
      override def traverseStm(stm: Stm): Unit = stm match {
        case _ => {
          indent += 3
          var str = ""
          for (i <- 0 until indent) {
            str += " "
          }
          super.traverseStm(stm)
          System.out.println(str + stm.toString())
          indent -= 3
        }
      }
    }
    printer.traverseBlock(block)
  }

  def funcToBlock[T](out: Exp[T])(implicit mT: Manifest[T]) = reifyEffects[T](out)
  def funcToBlock(out: DSLType) = reifyEffects[DSLType](out.rep)

  def findDefinitionByDeps(opSyms: Set[Sym[Any]], e: DefMN[Any] = null): List[Stm] = {
    var res = List.empty[Stm]
    globalDefs.foreach( stm => stm match {
      case TP(s, _) => {
        val symSet = syms(stm).toSet - s
        if (symSet.subsetOf(opSyms) && opSyms.subsetOf(symSet)) {
          res = stm :: res
        }
      }
      case _ =>
    })
    res.reverse
  }

  def findDefinitionByDeps(opSyms: List[Sym[Any]]): List[Stm] = {
    findDefinitionByDeps(opSyms.toSet)
  }

}
