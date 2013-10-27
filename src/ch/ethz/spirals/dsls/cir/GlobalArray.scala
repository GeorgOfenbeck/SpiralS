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

import scala.reflect.SourceContext
import scala.virtualization.lms.common.ArrayOpsExp

trait GlobalArrayOps extends scala.virtualization.lms.common.ArrayOps {

  def global_const_array_obj_new[T:Manifest](): Rep[Array[T]]

  // multiple definitions needed because implicits won't chain
  // not using infix here because apply doesn't work with infix methods
  implicit def varToGlobalArrayOps[T:Manifest](x: Var[Array[T]]) = new GlobalArrayOpsCls(readVar(x))
  implicit def repArrayToGlobalArrayOps[T:Manifest](a: Rep[Array[T]]) = new GlobalArrayOpsCls(a)
  implicit def arrayToGlobalArrayOps[T:Manifest](a: Array[T]) = new GlobalArrayOpsCls(unit(a))

  class GlobalArrayOpsCls[T:Manifest](a: Rep[Array[T]]){
    def apply(n: Rep[Int])(implicit pos: SourceContext) = array_apply(a, n)
    def update(n: Rep[Int], y: Rep[T])(implicit pos: SourceContext) = array_update(a,n,y)
    def length(implicit pos: SourceContext) = array_length(a)
    def foreach(block: Rep[T] => Rep[Unit])(implicit pos: SourceContext) = array_foreach(a, block)
    def sort(implicit pos: SourceContext) = array_sort(a)
    def map[B:Manifest](f: Rep[T] => Rep[B]) = array_map(a,f)
    def toSeq = array_toseq(a)
  }
}


trait GlobalArrayOpsExp extends ArrayOpsExp {

  case class GlobalConstArrayNew[T:Manifest](values: List[T]) extends Def[Array[T]] {
    val m = manifest[T]
  }

  object NewGlobalConstArray {
    def apply[T:Manifest](values: List[T]) = global_const_array_obj_new[T](values)
  }

  def global_const_array_obj_new[T:Manifest](values: List[T]) = reflectMutable(GlobalConstArrayNew(values))

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case c@GlobalConstArrayNew(values) => GlobalConstArrayNew (values)
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]] // why??

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case c@GlobalConstArrayNew(values) => global_const_array_obj_new (values)(c.m)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

  override def syms(e: Any): List[Sym[Any]] = e match {
    case GlobalConstArrayNew (_) => Nil
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case GlobalConstArrayNew (_) => Nil
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case GlobalConstArrayNew (_) => Nil
    case _ => super.symsFreq(e)
  }
}


