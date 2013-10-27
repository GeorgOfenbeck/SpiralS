/**
 *     _______  _______  ___   __      ____     Automatic
 *    / __/ _ \/  _/ _ \/ _ | / /     / __/     * Implementation
 *   _\ \/ ___// // , _/ __ |/ /__   _\ \       * Optimization
 *  /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation
 *                                              of DSP Algorithms
 *  https://bitbucket.org/GeorgOfenbeck/spirals
 *  SpiralS 0.1 Prototype - ETH Zurich
 *  Copyright (C) 2013  Georg Ofenbeck (ofenbeck@inf.ethz.ch)
 *                      Alen Stojanov  (astojanov@inf.ethz.ch)
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

package ch.ethz.spirals.datatypes

import ch.ethz.spirals.dsls.Intrinsics_DSL
import ch.ethz.spirals.dsls.cir.Trigonometry

import scala.virtualization.lms.common._

trait ImplicitDataTypes extends ArrayOpsExp with NumericOpsExpOpt with PrimitiveOpsExp with LiftNumeric with Trigonometry with Intrinsics_DSL { self =>

  /**
   * Type wrapper that expresses no presence of the Rep wrapper
   * @tparam T The equivalent type
   */
  type NoRep[T]  = T
  type Single[T] = T
  type RepPacked[A] = Rep[Packed[A]]

  object ImplicitOps
  {
    implicit def infixNumericRepOps[T](x: T)(implicit num: NumericOps[T]): NumericOps[T]#Ops = new num.Ops(x)

    trait NConv {
      protected def convert[U:Manifest:Numeric, T:Manifest:Numeric](u: U): T = manifest[T].toString match {
        case "Int"    => implicitly[Numeric[U]].toInt   (u).asInstanceOf[T]
        case "Long"   => implicitly[Numeric[U]].toLong  (u).asInstanceOf[T]
        case "Float"  => implicitly[Numeric[U]].toFloat (u).asInstanceOf[T]
        case "Double" => implicitly[Numeric[U]].toDouble(u).asInstanceOf[T]
        case _        => implicitly[Numeric[T]].fromInt(implicitly[Numeric[U]].toInt(u))
      }
    }

    implicit def packedNumericOps[T:Manifest:Numeric] = new NumericOps[Rep[Packed[T]]] with NConv {
      private val vsize = codegen.getInstructionSetVectorSize[T]
      def plus  (x : Rep[Packed[T]], y: Rep[Packed[T]]) = infix_vadd[T](x, y)
      def minus (x : Rep[Packed[T]], y: Rep[Packed[T]]) = infix_vsub[T](x, y)
      def times (x : Rep[Packed[T]], y: Rep[Packed[T]]) = infix_vmul[T](x, y)
      def fromDouble (x: Double) = infix_vset1[T](Const(convert[Double, T](x)), vsize)
    }

    protected abstract class NumericRepOps[T:Manifest:Numeric] extends NumericOps[Rep[T]] with NConv {
      def plus  (x : Rep[T], y: Rep[T]) = numeric_plus [T](x, y)
      def minus (x : Rep[T], y: Rep[T]) = numeric_minus[T](x, y)
      def times (x : Rep[T], y: Rep[T]) = numeric_times[T](x, y)
      def fromDouble (x: Double): Rep[T] = Const[T](convert[Double, T](x))
    }
    implicit object IntRep    extends NumericRepOps[Int]
    implicit object LongRep   extends NumericRepOps[Long]
    implicit object FloatRep  extends NumericRepOps[Float]
    implicit object DoubleRep extends NumericRepOps[Double]

    protected abstract class NumericNoRepOps[T:Numeric:Manifest] extends NumericOps[T] with NConv {
      def plus  (x : T, y: T) = implicitly[Numeric[T]].plus (x, y)
      def minus (x : T, y: T) = implicitly[Numeric[T]].minus(x, y)
      def times (x : T, y: T) = implicitly[Numeric[T]].times(x, y)
      def fromDouble (x: Double): T = convert[Double, T](x)
    }
    implicit object IntNoRep    extends NumericNoRepOps[Int]
    implicit object LongNoRep   extends NumericNoRepOps[Float]
    implicit object FloatNoRep  extends NumericNoRepOps[Float]
    implicit object DoubleNoRep extends NumericNoRepOps[Double]

    implicit object RepObject extends LiftOps[Rep]{
      def apply[T:Numeric:Manifest](x: T) = numericToNumericRep(x)
      def apply(x: Unit) = fresh[Unit]
      def apply[X[_], T:Numeric:Manifest] (exp: X[T])
          (implicit mE: Manifest[X[Any]]) : Rep[T] = exp match {
        case c: Const[_] => apply(c.x.asInstanceOf[T])
        case _ if (mA == mE) => exp.asInstanceOf[Rep[T]]
        case _ => throw new LiftOpsException(exp.toString + " can not be lifted")
      }
      def staged () = true
    }

    implicit object NoRepObject extends LiftOps[NoRep] {
      def apply[T:Numeric:Manifest](x: T) = x
      def apply(x: Unit) = Unit
      def apply[X[_], T:Numeric:Manifest] (exp: X[T])
          (implicit mE: Manifest[X[Any]]) : NoRep[T] = exp match {
        case c: Const[_] => apply(c.x.asInstanceOf[T])
        case _ if (mA == mE) => exp.asInstanceOf[NoRep[T]]
        case _ => throw new LiftOpsException(exp.toString + " can not be lifted")
      }
      def staged () = false
    }

    implicit def complexOps[T:NumericOps] = new ElementOps[ComplexElement, T] {
      def plus  (x: ComplexElement[T], y: ComplexElement[T]) = ComplexElement(x._re + y._re, x._im + y._im)
      def minus (x: ComplexElement[T], y: ComplexElement[T]) = ComplexElement(x._re - y._re, x._im - y._im)
      def times (x: ComplexElement[T], y: ComplexElement[T]) = {
        val m1 = x._re * y._re
        val m2 = x._im * y._im
        val m3 = x._re * y._im
        val m4 = x._im * y._re
        ComplexElement(m1 - m2, m3 + m4)
      }
      def create (v: List[T]) = v.size match {
        case 0 => val z = implicitly[NumericOps[T]].fromDouble(0); ComplexElement(z, z)
        case 1 => val z = implicitly[NumericOps[T]].fromDouble(0); ComplexElement(v(0), z)
        case _ => ComplexElement(v(0), v(1))
      }
      def extract (elem: ComplexElement[T]) = List(elem._re, elem._im)
    }

    implicit def realOps[T:NumericOps] = new ElementOps[RealElement, T] {
      def plus    (x: RealElement[T], y: RealElement[T]) = RealElement(x._re + y._re)
      def minus   (x: RealElement[T], y: RealElement[T]) = RealElement(x._re - y._re)
      def times   (x: RealElement[T], y: RealElement[T]) = RealElement(x._re * y._re)
      def create  (v: List[T]) = v.size match {
        case 0 => RealElement(implicitly[NumericOps[T]].fromDouble(0))
        case _ => RealElement(v(0))
      }
      def extract (elem: RealElement[T]) = List(elem._re)
    }

    implicit def stagedPackedArrayOps[T:Manifest] = new ArrayOps[Rep, Rep, Packed, NoRep, T] {
      def alloc (s: Rep[Int]): Rep[Array[T]] = array_obj_new(s)
      def apply (x: Rep[Array[T]], i: Rep[Int]): Rep[Packed[T]] = infix_vload[T](x, i, codegen.getInstructionSetVectorSize[T])
      def update(x: Rep[Array[T]], i: Rep[Int], y: Rep[Packed[T]]) = infix_vstore(x, i, y)
    }

    implicit def scalarPackedArrayOps[T:Manifest] = new ArrayOps[NoRep, Rep, Packed, Rep, T] {
      def alloc (s: NoRep[Int]): Array[Rep[T]] = new Array[Rep[T]](s)
      def apply (x: Array[Rep[T]], i: NoRep[Int]): Rep[Packed[T]] = {
        val r: Rep[Packed[T]] = null
          // TODO (Alen Stojanov): Write the implementation using infix_vset (or smth)
        r
      }
      def update(x: Array[Rep[T]], i: NoRep[Int], y: Rep[Packed[T]]) = {
        // TODO (Alen Stojanov): Write the implementation using infix_vget (or smth)
      }
    }

    implicit def packedScalarArrayOps[T:Manifest] = new ArrayOps[NoRep, Rep, Packed, RepPacked, T] {
      def alloc (s: NoRep[Int]): Array[RepPacked[T]] = new Array[RepPacked[T]](s)
      def apply (x: Array[RepPacked[T]], i: NoRep[Int]): Rep[Packed[T]] = x(i)
      def update(x: Array[RepPacked[T]], i: NoRep[Int], y: Rep[Packed[T]]) = x.update(i,y)
    }

    implicit def scalarSingleArrayOps[T:Manifest] = new ArrayOps[NoRep, Rep, Single, Rep, T] {
      def alloc (s: NoRep[Int]): Array[Rep[T]] = new Array[Rep[T]](s)
      def apply (x: Array[Rep[T]], i: NoRep[Int]): Rep[T] = x(i)
      def update(x: Array[Rep[T]], i: NoRep[Int], y: Rep[T]) = x.update(i,y)
    }

    implicit def stagedSingleArrayOps[T:Manifest] = new ArrayOps[Rep, Rep, Single, NoRep, T] {
      def alloc (s: Rep[Int]): Rep[Array[T]] = array_obj_new(s)
      def apply (x: Rep[Array[T]], i: Rep[Int]): Rep[T] = array_apply(x,i)
      def update(x: Rep[Array[T]], i: Rep[Int], y: Rep[T]) = array_update(x,i,y)
    }

    implicit def vectorElementOpsSingle[E[_], T: Manifest](implicit eops: ElementOps[E, Rep[Single[T]]]) = {
      new VectorElementOps[E, Rep, Single, T]() {
        def length () = 1
        def vset1    (x: E[Rep[T]])(implicit e: ElementOps[E, Rep[T]]) = x
        def hadd     (a: E[Rep[Single[T]]], b: E[Rep[Single[T]]]) = eops.plus(a, b)
        def permute2 (a: E[Rep[Single[T]]], b: E[Rep[Single[T]]], mask: Int) = if ( mask == 1 ) a else b
      }
    }

    implicit def vectorElementOpsPacked[E[_], T: Manifest](implicit eops: ElementOps[E, Rep[Packed[T]]]) = {
      new VectorElementOps[E, Rep, Packed, T]() {
        def length() = codegen.getInstructionSetVectorSize[T]
        def vset1(x: E[Rep[T]])(implicit e: ElementOps[E, Rep[T]]) = eops.create(
          e.extract(x).map(t => infix_vset1[T](t, codegen.getInstructionSetVectorSize[T]))
        )
        def hadd(a: E[Rep[Packed[T]]], b: E[Rep[Packed[T]]]) = eops.create(
          (eops.extract(a) zip eops.extract(b)).map(z => infix_hadd[T](z._1, z._2))
        )
        def permute2 (a: E[Rep[Packed[T]]], b: E[Rep[Packed[T]]], mask: Int) = eops.create(
          (eops.extract(a) zip eops.extract(b)).map(z => infix_permute2[T](z._1, z._2, mask))
        )
      }
    }

  }

  case class ComplexElement[T](_re: T, _im: T)
  case class RealElement   [T](_re: T)
}
