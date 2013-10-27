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

/* AUTO-GENERATED FILE */

package ch.ethz.spirals.util

import ch.ethz.spirals.datatypes.CVector

abstract class GeneralCVectorMatcher1[VStaged[_], VScalar[_], EStaged[_], EScalar[_], RStaged[_], RScalar[_], PStaged[_], PScalar[_], TStaged, TScalar] {
  type EitherX = Either[CVector[VStaged, EStaged, RStaged, PStaged, TStaged], CVector[VScalar, EScalar, RScalar, PScalar, TScalar]]
  def f[A[_], B[_], C[_], D[_], E](a: CVector[A, B, C, D, E]) : Unit
  def apply(a: EitherX) = (a) match {
    case (Left(b)) => f(b)
    case (Right(b)) => f(b)
  }
}

abstract class GeneralCVectorMatcher2[VStaged[_], VScalar[_], EStaged[_], EScalar[_], RStaged[_], RScalar[_], PStaged[_], PScalar[_], TStaged, TScalar] {
  type EitherX = Either[CVector[VStaged, EStaged, RStaged, PStaged, TStaged], CVector[VScalar, EScalar, RScalar, PScalar, TScalar]]
  def f[A[_], B[_], C[_], D[_], E, F[_], G[_], H[_], I[_], J](a: CVector[A, B, C, D, E], b: CVector[F, G, H, I, J]) : Unit
  def apply(a: EitherX, b:EitherX) = (a, b) match {
    case (Left(c), Left(d)) => f(c, d)
    case (Left(c), Right(d)) => f(c, d)
    case (Right(c), Left(d)) => f(c, d)
    case (Right(c), Right(d)) => f(c, d)
  }
}

abstract class GeneralCVectorMatcher3[VStaged[_], VScalar[_], EStaged[_], EScalar[_], RStaged[_], RScalar[_], PStaged[_], PScalar[_], TStaged, TScalar] {
  type EitherX = Either[CVector[VStaged, EStaged, RStaged, PStaged, TStaged], CVector[VScalar, EScalar, RScalar, PScalar, TScalar]]
  def f[A[_], B[_], C[_], D[_], E, F[_], G[_], H[_], I[_], J, K[_], L[_], M[_], N[_], O](a: CVector[A, B, C, D, E], b: CVector[F, G, H, I, J], c: CVector[K, L, M, N, O]) : Unit
  def apply(a: EitherX, b:EitherX, c:EitherX) = (a, b, c) match {
    case (Left(d), Left(e), Left(g)) => f(d, e, g)
    case (Left(d), Left(e), Right(g)) => f(d, e, g)
    case (Left(d), Right(e), Left(g)) => f(d, e, g)
    case (Left(d), Right(e), Right(g)) => f(d, e, g)
    case (Right(d), Left(e), Left(g)) => f(d, e, g)
    case (Right(d), Left(e), Right(g)) => f(d, e, g)
    case (Right(d), Right(e), Left(g)) => f(d, e, g)
    case (Right(d), Right(e), Right(g)) => f(d, e, g)
  }
}
