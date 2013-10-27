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

package ch.ethz.spirals.datatypes

trait DataTypeFactories extends ComplexDataTypes  { self =>

  protected class AbstractFactories { import ImplicitOps._

    class InterleavedComplexFactory[T:Numeric:Manifest](implicit nops: NumericOps[Rep[T]])
      extends DataTypeFactory[Rep, NoRep, ComplexElement, ComplexElement, Rep, Single, T, T]
    {
      type Packed[A] = self.Packed[A]
      def createPacked (size: Int) = new InterleavedAComplexVector[NoRep, RepPacked, Rep, Packed, T](size)
      def createScalar (size: Int) = new InterleavedAComplexVector[NoRep, Rep, Rep, Single, T](size)
      def createStaged (size: Int) = new InterleavedAComplexVector[Rep, NoRep, Rep, Single, T](size)
      def createStaged (size: Int, rep: Rep[Array[T]]) = new InterleavedAComplexVector[Rep, NoRep, Rep, Single, T](size, rep)
    }

    class RealNumberFactory[T:Numeric:Manifest](implicit nops: NumericOps[Rep[T]])
      extends DataTypeFactory[Rep, NoRep, RealElement, RealElement, Rep, Single, T, T]
    {
      type Packed[A] = self.Packed[A]
      def createPacked (size: Int) = new AVector[NoRep, Rep, RepPacked, Packed, T](size)
      def createScalar (size: Int) = new AVector[NoRep, Rep, Rep, Single, T](size)
      def createStaged (size: Int) = new AVector[Rep, Rep, NoRep, Single, T](size)
      def createStaged (size: Int, rep: Rep[Array[T]]) = new AVector[Rep, Rep, NoRep, Single, T](size, rep)
    }
  }

  object ComplexNumberFactories extends AbstractFactories { import ImplicitOps._
    object InterleavedDoubles   extends InterleavedComplexFactory[Double]
    object InterleavedFloats    extends InterleavedComplexFactory[Float]
    object InterleavedInts      extends InterleavedComplexFactory[Int]
  }

  object RealNumberFactories extends AbstractFactories { import ImplicitOps._
    object RealNumberDoubles extends RealNumberFactory[Double]
    object RealNumberFloats  extends RealNumberFactory[Float]
    object RealNumberInts    extends RealNumberFactory[Int]
  }



}


