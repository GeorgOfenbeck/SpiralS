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

import ch.ethz.spirals.util.MathUtilities._

abstract class CVector[VectorRep[_], ElementClass[_], R[_], P[_], T](private var chunked: Boolean = true) (implicit
  val vrep: LiftOps[VectorRep],
  val erep: ElementOps[ElementClass, R[P[T]]],
  val nrep: NumericOps[R[P[T]]],
  val vops: VectorElementOps[ElementClass, R, P, T]
) {
  type Packed[A]
  def apply  (i: VectorRep[Int]): ElementClass[R[P[T]]]
  def create (s: VectorRep[Int]): CVector[VectorRep, ElementClass, R, P, T]
  def update (i: VectorRep[Int], y: ElementClass[R[P[T]]])
  def size   (): VectorRep[Int]
  def toSIMD () : CVector[VectorRep, ElementClass, R, Packed, T]
}

abstract class VectorElementOps[ElementClass[_], R[_], P[_], T](implicit
  val eops: ElementOps[ElementClass, R[P[T]]]
) {
  def length(): Int
  def hadd     (a: ElementClass[R[P[T]]], b: ElementClass[R[P[T]]]): ElementClass[R[P[T]]]
  def permute2 (a: ElementClass[R[P[T]]], b: ElementClass[R[P[T]]], mask: Int): ElementClass[R[P[T]]]
  def vset1    (a: ElementClass[R[T]])(implicit e: ElementOps[ElementClass, R[T]]): ElementClass[R[P[T]]]
}

abstract class ElementOps[ElementClass[_], T: NumericOps]  {

  lazy val numeric = implicitly[NumericOps[T]]

  def plus    (x: ElementClass[T], y: ElementClass[T]): ElementClass[T]
  def minus   (x: ElementClass[T], y: ElementClass[T]): ElementClass[T]
  def times   (x: ElementClass[T], y: ElementClass[T]): ElementClass[T]
  def create  (l: List[T]): ElementClass[T]
  def create  (r: T, i: T): ElementClass[T] = create(List(r, i))
  def extract (x: ElementClass[T]): List[T]
  def mix     [E2[_], T2](rhs: ElementOps[E2, T2]) = new ElementOpsMix(this, rhs)

  protected class ElementOpsMix[E1[_], E2[_], T1, T2](lhs: ElementOps[E1, T1], rhs: ElementOps[E2, T2]) {
    val (m1, m2) = (lhs.numeric.m, rhs.numeric.m)
    assert(m1 == m2, "Underlying types must be equal: " + m1.toString() + " != " + m1.toString())
    def convert(y: E2[T2]): E1[T1] = lhs.create(rhs.extract(y).asInstanceOf[List[T1]])
    def plus   (x: E1[T1], y: E2[T2]): E1[T1] = lhs.plus (x, convert(y))
    def minus  (x: E1[T1], y: E2[T2]): E1[T1] = lhs.minus(x, convert(y))
    def times  (x: E1[T1], y: E2[T2]): E1[T1] = lhs.times(x, convert(y))
  }
}

abstract class NumericOps[T:Manifest] extends TrigonometryOps [T] {	//inspired by Numeric

  lazy val m = implicitly[Manifest[T]]

  def plus       (x: T, y: T) : T
  def minus      (x: T, y: T) : T
  def times      (x: T, y: T) : T
  def fromDouble (x: Double)  : T
  class Ops(lhs: T) {
    def +(rhs: T) = plus (lhs, rhs)
    def -(rhs: T) = minus(lhs, rhs)
    def *(rhs: T) = times(lhs, rhs)
  }
  def mkNumericOps(lhs: T): Ops = new Ops(lhs)
}

/**
 *
 * @tparam V Staged array or not
 * @tparam E Element staged or not
 * @tparam P Packed or single
 * @tparam A Staged/non-staged and/or packed/single elements
 * @tparam T Primitives type
 */
abstract class ArrayOps[V[_], R[_], P[_], A[_], T] {
  def alloc  (s: V[Int]): V[Array[A[T]]]
  def apply  (x: V[Array[A[T]]], i: V[Int]): R[P[T]]
  def update (x: V[Array[A[T]]], i: V[Int], y: R[P[T]])
}

abstract class LiftOps[R[_]](implicit val mA: Manifest[R[Any]]) {
  def apply  (x : Unit) : R[Unit]
  def apply  [T:Numeric:Manifest] (x:T) : R[T]
  def apply  [X[_], T:Numeric:Manifest] (exp: X[T])(implicit mE: Manifest[X[Any]]): R[T]
  def staged (): Boolean
}

/**
 *
 * @tparam VStaged Represents whether the staged version of the vector is a Rep or NoRep (most cases Rep)
 * @tparam VScalar Represents whether the scalar version of the vector is a Rep or NoRep (most cases NoRep)
 * @tparam EStaged Represents the element container of the vector
 * @tparam R       Represents whether the underlying type is staged or not
 * @tparam TStaged Underlying computation type of the staged version
 * @tparam TScalar Underlying computation type of the scalar version
 */
abstract class DataTypeFactory[VStaged[_], VScalar[_], EStaged[_], EScalar[_], R[_], NP[_], TStaged: Manifest, TScalar: Manifest]() {
  type Packed[A]
  def createPacked (size: Int): CVector[VScalar, EStaged, R, Packed, TStaged]
  def createScalar (size: Int): CVector[VScalar, EScalar, R, NP, TScalar]
  def createStaged (size: Int): CVector[VStaged, EStaged, R, NP, TStaged]
  def createStaged (size: Int, arr: VStaged[Array[TStaged]]): CVector[VStaged, EStaged, R, NP, TStaged]
}

/**
 * Lift Ops Exception. Fires when a certain expression can not be lifted.
 * @param msg Expresses the reason why the expression can not be lifted
 */
class LiftOpsException(msg: String) extends Exception (msg)




