/**
 *     _______  _______  ___   __      ____     Automatic
 *    / __/ _ \/  _/ _ \/ _ | / /     / __/     * Implementation
 *   _\ \/ ___// // , _/ __ |/ /__   _\ \       * Optimization
 *  /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation
 *                                              of DSP Algorithms
 *  https://bitbucket.org/GeorgOfenbeck/spirals
 *  SpiralS 0.1 Prototype - ETH Zurich
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

package ch.ethz.spirals.dsls

import virtualization.lms.common._
import reflect.SourceContext
import ch.ethz.spirals.dsls.cir.CIRNumericExp
import ch.ethz.spirals.util.{SubstitutionTransformer, DSLUtils}
import ch.ethz.spirals.datatypes.DSLBaseTypes

trait SigmaSPLBase_Base extends Base with DSLBaseTypes with FunctionsExp with DSLUtils {

  implicit def SigmaSPLBasetoRep (i: Vector): Rep[Vector] = unit(i)
  implicit def InttoRep (i: Int): Rep[Int] = unit(i)

  abstract class IndexMapping
  abstract class Tag

  case class Unroll extends Tag

  def H (fragsize: Rep[Int], stride: Rep[Int], range: Int, domain: Int): Rep[IndexMapping] = infix_im_h(fragsize, stride, range, domain)

  def infix_im_h(fragsize: Rep[Int], stride: Rep[Int], range: Int, domain: Int): Rep[IndexMapping]
  def infix_im_l(fragsize: Rep[Int], stride: Rep[Int], range: Int, domain: Int): Rep[IndexMapping]
  def infix_im_compose(x: Rep[IndexMapping], y: Rep[IndexMapping], range: Int, domain: Int): Rep[IndexMapping]

  def G (im: Rep[IndexMapping], x: Vector): Vector = getDSLVector(infix_gather  (im, x.getRep))
  def S (im: Rep[IndexMapping], x: Vector): Vector = getDSLVector(infix_scatter (im, x.getRep))

  def tag_start (x: Vector, tag: Tag) : Vector = getDSLVector(infix_tagstart(x.getRep, tag))
  def tag_end   (x: Vector, tag: Tag) : Vector = getDSLVector(infix_tagend(x.getRep, tag))
  def directsum (x: Vector, y: Vector): Vector = getDSLVector(infix_directsum(x.getRep, y.getRep))
  def sum       (x: Vector, y: Vector): Vector = getDSLVector(infix_sum(x.getRep, y.getRep))
  def dot       (x: Vector, y: Vector): Vector = getDSLVector(infix_dot(x.getRep, y.getRep))

  def sigmasum (range: Rep[Int], f: Rep[Int] => Vector): Vector = sigmasum(0, range, f)
  def sigmasum (start: Rep[Int], end: Rep[Int], f: Rep[Int] => Vector): Vector = {
    val iter = fresh[Int]
    getDSLVector(infix_sigmasum(start, end, iter, reifyEffects(f(iter).getRep)))
  }

  def sigma (range: Rep[Int], f: Rep[Int] => Vector): Vector = sigma(0, range, f)
  def sigma (start: Rep[Int], end: Rep[Int], f: Rep[Int] => Vector): Vector = {
    val iter = fresh[Int]
    getDSLVector(infix_sigma(start, end, iter, reifyEffects(f(iter).getRep)))
  }

  def infix_+  (x:Vector, y:Vector): Vector = sum(x, y)
  def infix_++ (x:Vector, y:Vector): Vector = directsum(x, y)
  def infix_unroll = Unroll ()

  def infix_tagstart  (body: Rep[Vector], tagType: Tag): Rep[Vector]
  def infix_tagend    (body: Rep[Vector], tagType: Tag): Rep[Vector]
  def infix_gather    (im: Rep[IndexMapping], x: Rep[Vector]): Rep[Vector]
  def infix_scatter   (im: Rep[IndexMapping], x: Rep[Vector]): Rep[Vector]
  def infix_sigma     (start: Rep[Int], end: Rep[Int], v: Sym[Int], x: Block[Vector]) : Rep[Vector]
  def infix_sigmasum  (start: Rep[Int], end: Rep[Int], v: Sym[Int], body: Block[Vector]) : Rep[Vector]
  def infix_directsum (x: Rep[Vector], y: Rep[Vector]): Rep[Vector]

  def infix_sum       (x: Rep[Vector], y: Rep[Vector]): Rep[Vector]
  def infix_dot       (x: Rep[Vector], y: Rep[Vector]): Rep[Vector]

}

trait SigmaSPLBase_Exp extends SigmaSPLBase_Base with CIRNumericExp { self =>

  abstract class DefIM (range: Int, domain : Int) extends Def[IndexMapping] { val m_domain: Int = domain; val m_range : Int = range }

  case class IM_H       (fragsize: Exp[Int], stride: Exp[Int], range: Int, domain: Int)       extends DefIM(range, domain)
  case class IM_L       (fragsize: Exp[Int], stride: Exp[Int], range: Int, domain: Int)       extends DefIM(range, domain)
  case class IM_Compose (x: Exp[IndexMapping], y: Exp[IndexMapping], range: Int, domain: Int) extends DefIM(range, domain)

  def infix_im_h(fragsize: Exp[Int], stride: Exp[Int], range: Int, domain: Int): Exp[IndexMapping] = IM_H(fragsize, stride, range, domain)
  def infix_im_l(fragsize: Exp[Int], stride: Exp[Int], range: Int, domain: Int): Exp[IndexMapping] = IM_L(fragsize, stride, range, domain)
  def infix_im_compose(x: Exp[IndexMapping], y: Exp[IndexMapping], range: Int, domain: Int): Exp[IndexMapping] = IM_Compose(x, y, range, domain)


  def getIMSize (im: Exp[IndexMapping]): (Int,Int) = findDefinition(im.asInstanceOf[Sym[IndexMapping]]) match {
    case Some(stm) => stm match {
      case TP(_, e: DefIM) => (e.m_range, e.m_domain)
      case _ => (-1,-1)
    }
    case None => (-1,-1)
  }

  case class TagStart   (body: Exp[Vector], tagType: Tag)       extends Def[Vector]
  case class TagEnd     (body: Exp[Vector], tagType: Tag)       extends Def[Vector]
  case class Gather     (im: Exp[IndexMapping], x: Exp[Vector]) extends Def[Vector]
  case class Scatter    (im: Exp[IndexMapping], x: Exp[Vector]) extends Def[Vector]
  case class Dot        (x: Exp[Vector], y: Exp[Vector])        extends Def[Vector]
  case class Sum        (x: Exp[Vector], y: Exp[Vector])        extends Def[Vector]
  case class DirectSum  (x: Exp[Vector], y: Exp[Vector])        extends Def[Vector]
  case class Sigma      (start: Exp[Int], end: Exp[Int], i: Sym[Int], body: Block[Vector])  extends Def[Vector]
  case class SigmaSum   (start: Exp[Int], end: Exp[Int], i: Sym[Int], body: Block[Vector])  extends Def[Vector]

  def infix_tagend (x: Exp[Vector], tagType: Tag): Exp[Vector] = {
    val xVec = getDSLVector(x)
    val xsize = xVec.size
    new Vector(TagEnd(x, tagType), xsize).getRep
  }
  def infix_tagstart (x: Exp[Vector], tagType: Tag): Exp[Vector] = {
    val xVec = getDSLVector(x)
    val xsize = xVec.size
    new Vector(TagStart(x, tagType), xsize).getRep
  }
  def infix_directsum (x: Exp[Vector], y: Exp[Vector]): Exp[Vector] = {
    val xVec = getDSLVector(x)
    val yVec = getDSLVector(y)
    val (xsize, ysize) = (xVec.size, yVec.size)
    assert(xsize == ysize, "DirectSum(" + xVec.toString() + ", " + yVec.toString() + "): Vectors must have equal size")
    new Vector(DirectSum(x, y), xsize).getRep
  }
  def infix_sum (x: Exp[Vector], y: Exp[Vector]): Exp[Vector] = {
    val xsize = getDSLVector(x).size
    val ysize = getDSLVector(y).size
    assert(xsize == ysize, "SUM(" + getDSLVector(x).toString() + ", " + getDSLVector(y).toString() + "): Vectors must have equal size")
    if ( xsize == 0 ) y else if ( ysize == 0 ) x else {
      new Vector(Sum(x, y), xsize).getRep
    }
  }
  def infix_dot (x: Exp[Vector], y: Exp[Vector]): Exp[Vector] = {
    val xsize = getDSLVector(x).size
    val ysize = getDSLVector(y).size
    assert(xsize == ysize, "DOT(" + getDSLVector(x).toString() + ", " + getDSLVector(y).toString() + "): Vectors must have equal size")
    if ( xsize == 0 ) y else if ( ysize == 0 ) x else {
      new Vector(Dot(x, y), xsize).getRep
    }
  }
  def infix_gather (im: Exp[IndexMapping], x: Exp[Vector]): Exp[Vector] = {
    val (range, domain) = getIMSize(im)
    assert(range > 0 && domain > 0, "Gather must have a size larger than zero")
    new Vector(Gather (im, x), range).getRep
  }
  def infix_scatter   (im: Exp[IndexMapping], x: Exp[Vector]): Exp[Vector] = {
    val (range, domain) = getIMSize(im)
    assert(range > 0 && domain > 0, "Scatter must have a size larger than zero")
    new Vector(Scatter (im, x), domain).getRep
  }
  def infix_sigma     (start: Exp[Int], end: Exp[Int], v: Sym[Int], body: Block[Vector]): Exp[Vector] = {
    val size = getDSLVector(body.res).size
    if ( size == 0 ) body.res else (start, end) match {
      case (Const(vs), Const(ve)) if (ve - vs == 1) => {
        val t = new SubstitutionTransformer { val IR: self.type = self }
        t.register(v, start)
        t.transformBlock(body).res
      }
      case (Const(vs), Const(ve)) if (ve <= vs) => throw new RuntimeException("sigma(" + start + "," + end + "," + v + "," + body + ") is impossible")
      case _ => {
        val rep = reflectEffect[Vector](Sigma(start, end, v, body), summarizeEffects(body).star)
        new Vector(rep, size).getRep
      }
    }
  }
  def infix_sigmasum  (start: Exp[Int], end: Exp[Int], v: Sym[Int], body: Block[Vector]): Exp[Vector] = {
    val size = getDSLVector(body.res).size
    if ( size == 0 ) body.res else (start, end) match {
      case (Const(vs), Const(ve)) if (ve - vs == 1) => {
      	val t = new SubstitutionTransformer { val IR: self.type = self }
        t.register(v, start)
        t.transformBlock(body).res
      }
      case (Const(vs), Const(ve)) if (ve <= vs) => throw new RuntimeException("SigmaSum(" + start + "," + end + "," + v + "," + body + ") is impossible")
      case _ => {
        val rep = reflectEffect[Vector](SigmaSum(start, end, v, body), summarizeEffects(body).star)
        new Vector(rep, size).getRep
      }
    }
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Sigma(start, end, i, y) => i :: effectSyms(y)
    case SigmaSum(start, end, i, y) => i :: effectSyms(y)
    case _ => super.boundSyms(e)
  }

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = {
    (e match {
      case e@Sigma(start, end, i, y) => Sigma(f(start), f(end), f(i).asInstanceOf[Sym[Int]], f(y))
      case e@SigmaSum(start, end, i, y) => SigmaSum(f(start), f(end), f(i).asInstanceOf[Sym[Int]], f(y))
      case _ => super.mirrorDef(e,f)
    }).asInstanceOf[Def[A]]
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case TagEnd(body, tag)      => infix_tagend(f(body), tag)
    case TagStart(body, tag)    => infix_tagstart(f(body), tag)
    case Sum(x, y)              => infix_sum(f(x), f(y))
    case Dot(x, y)              => infix_dot(f(x), f(y))
    case DirectSum(x, y)        => infix_directsum(f(x), f(y))
    case Sigma (s, e, i, b)     => infix_sigma(f(s), f(e), f(i).asInstanceOf[Sym[Int]], f(b))
    case SigmaSum(s, e, i, b)   => infix_sigmasum(f(s), f(e), f(i).asInstanceOf[Sym[Int]], f(b))
    case Gather (im, x)         => infix_gather(f(im), f(x))
    case Scatter (im, x)        => infix_scatter(f(im), f(x))
    case IM_H(b, s, range, domain)       => infix_im_h(f(b), f(s), range, domain)
    case IM_L(b, s, range, domain)       => infix_im_l(f(b), f(s), range, domain)
    case IM_Compose(x, y, range, domain) => infix_im_compose(f(x), f(y), range, domain)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

class SigmaSPLBase extends SigmaSPLBase_Exp with BaseFatExp with EffectExp with LoopsFatExp with IfThenElseFatExp {}
