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

import ch.ethz.spirals.dsls._
import ch.ethz.spirals.util.Utilities._
import ch.ethz.spirals.conf._
import ch.ethz.spirals.dsls.cir.GlobalArrayOpsExp

import scala.virtualization.lms.common._
import virtualization.lms.internal._
import collection.mutable.HashMap
import scala.Some
import java.io._
import org.bridj._

trait CUnparser extends CCodegen  {

  val IR: Expressions with EffectExp
  import IR._

  var includeHeaders = List.empty[String]
  var validReturns = List.empty[Sym[Any]]

  def getArrayInnerTypeManifest[A](m: Manifest[A]) = {
    Manifest.classType(m.erasure.getComponentType)
  }

  override def remap[A](m: Manifest[A]) : String = {
    if (m.erasure == classOf[Variable[Any]] ) {
      remap(m.typeArguments.head)
    }
    else {
      if (m.erasure.isArray()) {
        val mp = getArrayInnerTypeManifest(m)
        remap(mp) + "*"
      } else {
        m.toString match {
          case "double" => "double "
          case "float" => "float "
          case "int" => "int "
          case _ => super.remap(m)
        }
      }
    }
  }

  def checkReturnValue(block: Block[Any]) = {
    validReturns = List.empty[Sym[Any]]
  }

  def writeIncludeHeaders (out: PrintWriter) = {
    for ( header <- includeHeaders ) {
      out.println("#include <" + header + ">")
    }
  }

  def emitGlobalNodes (block: Block[Any]) = {}

  def emitSource[B](f: List[Exp[Any]] => Exp[B], functionName: String, out: PrintWriter)
      (implicit mList: List[Manifest[Any]], mB:Manifest[B]): List[(Sym[Any], Any)] = {
    emitTransformedSource(f, functionName, out, List.empty[CodeTransformer])
  }

  override def emitSource[A,B](f: Exp[A] => Exp[B], functionName: String, out: PrintWriter)
      (implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    val func: (List[Exp[Any]] => Exp[B]) = (in: List[Exp[Any]]) => f(in(0).asInstanceOf[Exp[A]])
    implicit val mList = List(mA).asInstanceOf[List[Manifest[Any]]]
    emitTransformedSource(func, functionName, out, List.empty[CodeTransformer])
  }

  def emitTransformedBlock[B](block:(List[Sym[Any]], Block[B]), functionName: String, out: PrintWriter, formatSource: Boolean = true)
      (implicit mList: List[Manifest[Any]], mB: Manifest[B]) = {
    val stringOutput = new StringWriter()
    val stringWriter = new PrintWriter(stringOutput)
    //includeHeaders = List.empty[String]
    includeHeaders = List("math.h")
    validReturns = List.empty[Sym[Any]]
    val (x, y) = (block._1, block._2)
    val staticData = getFreeDataBlock(y)
    checkReturnValue(y)
    writeIncludeHeaders(stringWriter)
    withStream(stringWriter) {
      emitGlobalNodes (y)
      stream.println(remap(mB) + " " + functionName + "(" + (mList zip x).map(m => remap(m._1) + quote(m._2)).mkString(", ") + ") {")
      emitBlock(y)
      stream.println("}")
    }
    stringWriter.flush()
    if (formatSource && Config().codegenconfig.useCodeFormatter) {
      out.write(indent(stringOutput.toString));
    } else {
      out.write(stringOutput.toString);
    }
    out.flush()
    staticData
  }

  def emitTransformedSource[B](f: List[Exp[Any]] => Exp[B], functionName: String, out: PrintWriter, transformers: List[CodeTransformer], indent: Boolean = true)
      (implicit mList: List[Manifest[Any]], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    var p = List.empty[Exp[Any]]
    mList.foreach( m => { p = p :+ fresh(m) } )
    var q = reifyBlock[B](f(p))
    for ( transformer <- transformers ) {
      val tmp = transformer.transform[B]((p.asInstanceOf[List[transformer.IR.Sym[Any]]], q.asInstanceOf[transformer.Block[B]]))
      p = tmp._1.asInstanceOf[List[CUnparser.this.IR.Sym[Any]]]
      q = tmp._2.asInstanceOf[CUnparser.this.Block[B]]
    }
    val (x, y) = (p.asInstanceOf[List[IR.Sym[Any]]], q)
    emitTransformedBlock((x, y), functionName, out, indent)
  }

  override def quote(x: Exp[Any]) : String = x match {
    case s@Sym(n) if (s.tp.toString() == "Int") => "i"+n
    case _ => super.quote(x)
  }
}
