/**
 *     _______  _______  ___   __      ____     Automatic
 *    / __/ _ \/  _/ _ \/ _ | / /     / __/     * Implementation
 *   _\ \/ ___// // , _/ __ |/ /__   _\ \       * Optimization
 *  /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation
 *    of DSP Algorithms
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

import shapeless.TypeOperators._

trait ComplexDataTypes extends ImplicitDataTypes { self =>

  class InterleavedAComplexVector[V[_], A[_], R[_], P[_], T: Manifest](s: V[Int], d: V[Array[A[T]]] = null)(implicit
    // Template operators
    aops: ArrayOps[V, R, P, A, T],
    vrep: LiftOps[V],
    erep: ElementOps[ComplexElement, R[P[T]]],
    nrep: NumericOps[R[P[T]]],
    vops: VectorElementOps[ComplexElement, R, P, T],
    irep: NumericOps[V[Int]],
    // Packed operators
    aopsPacked: ArrayOps[V, R, Packed, A, T],
    erepPacked: ElementOps[ComplexElement, R[Packed[T]]],
    nrepPacked: NumericOps[R[Packed[T]]],
    vopsPacked: VectorElementOps[ComplexElement, R, Packed, T]
  ) extends CVector[V, ComplexElement, R, P, T] {

    type Packed[X] = self.Packed[X]

    private val data = if ( d == null ) aops.alloc(irep.times(s, irep.fromDouble(2))) else d
    private val one = irep.fromDouble(1)
    private val two = irep.fromDouble(2)

    def toSIMD (): CVector[V, ComplexElement, R, Packed, T] = new InterleavedAComplexVector[V, A, R, Packed, T](s, data)

    def create (n: V[Int]) = new InterleavedAComplexVector[V, A, R, P, T](n, aops.alloc(irep.times(n, irep.fromDouble(2))))
    def apply(i: V[Int]): ComplexElement[R[P[T]]] = new ComplexElement (
      _re = aops.apply(data, irep.times(i, two)),
      _im = aops.apply(data, irep.plus(irep.times(i, two), one))
    )
    def update(i: V[Int], y: ComplexElement[R[P[T]]]) = {
      aops.update(data, irep.times(i, two), y._re)
      aops.update(data, irep.plus(irep.times(i, two), one), y._im)
    }
    def returnrep(): V[Array[A[T]]] = data
    def size() = s
    override def toString = s"SplitAComplexVector($data)"
  }

  class AVector[V[_], R[_], A[_], P[_], T: Manifest] (s: V[Int], d: V[Array[A[T]]] = null)(implicit
    // Template operators
    aops: ArrayOps[V, R, P, A, T],
    vrep: LiftOps[V],
    nrep: NumericOps[R[P[T]]],
    erep: ElementOps[RealElement, R[P[T]]],
    vops: VectorElementOps[RealElement, R, P, T],
    // Packed operators
    aopsPacked: ArrayOps[V, R, Packed, A, T],
    nrepPacked: NumericOps[R[Packed[T]]],
    erepPacked: ElementOps[RealElement, R[Packed[T]]],
    vopsPacked: VectorElementOps[RealElement, R, Packed, T]

  ) extends CVector[V, RealElement, R, P, T] {

    type Packed[X] = self.Packed[X]
    val data = if ( d == null ) aops.alloc(s) else d

    def toSIMD  (): CVector[V, RealElement, R, Packed, T] = new AVector[V, R, A, Packed, T](s, data)
    def create  (s: V[Int]) = new AVector[V, R, A, P, T](s, aops.alloc(s))
    def apply   (i: V[Int]) = RealElement(aops.apply(data, i))
    def update  (i: V[Int], y: RealElement[R[P[T]]]) = aops.update(data, i, y._re)
    def size () = s
    override def toString = s"AVector($data)"
  }

  // This represents a root of Unity
  case class E[T](val n : Int) (implicit ev: NumericOps[T]){
    private def yieldk(n:Int) = {    //TODO - find short form for return value
    def tmp () = {
      for (k <- 0 until n
           // this if checks if x^t becomes 1 before n==t, this is e.g. the
           // case for 2nd root of unity of 4 where it becomes 1 at x^2
           if (for (t <- 2 until n-1
        if (Math.cos(2*math.Pi*k*t/n) == 1)
           ) yield 1).isEmpty
      )
      yield k
    }
      tmp.last
    }
    val store = yieldk(n)
    def re(p: Int): T = {
      val x = ev.CosPi(2.0 * p * store, n)
      x
    }
    def im(p: Int): T = ev.times(ev.SinPi(2.0 * p * store, n),ev.fromDouble(-1.0))
  }

}
