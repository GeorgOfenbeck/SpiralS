/**
 *     _______  _______  ___   __      ____     Automatic
 *    / __/ _ \/  _/ _ \/ _ | / /     / __/     * Implementation
 *   _\ \/ ___// // , _/ __ |/ /__   _\ \       * Optimization
 *  /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation
 *                                              of DSP Algorithms
 *  https://bitbucket.org/GeorgOfenbeck/spirals
 *  SpiralS 0.1 Prototype
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

package ch.ethz.spirals.rewrites

import ch.ethz.spirals.dsls._
import ch.ethz.spirals.util._

import scala.virtualization.lms.common.ForwardTransformer

import collection.immutable.HashMap
import scala.Enumeration
import ch.ethz.spirals.datatypes._
import scala.Some

abstract class SigmaSPLBase2CIR [VStaged[_], VScalar[_], EStaged[_], EScalar[_], R[_], NP[_], TStaged, TScalar]
  (DTF: DataTypeFactory[VStaged, VScalar, EStaged, EScalar, R, NP, TStaged, TScalar])
    (implicit val mAT: Manifest[Array[TStaged]]) extends NestedBlockTraversalOrdered { self =>

  val IR: SigmaSPLBase
  val C: CIR_DSL
  import IR._

  object TraversalModeEnum extends Enumeration {
    type TraversalModeEnum = Value
    val Translate, Bind, Allocate, Unrolling = Value
  }
  import TraversalModeEnum._
  var traversalMode: TraversalModeEnum = Translate

  abstract class MatchCVector1 extends GeneralCVectorMatcher1 [VStaged, VScalar, EStaged, EScalar, R, R, NP, NP, TStaged, TScalar]
  abstract class MatchCVector2 extends GeneralCVectorMatcher2 [VStaged, VScalar, EStaged, EScalar, R, R, NP, NP, TStaged, TScalar]
  abstract class MatchCVector3 extends GeneralCVectorMatcher3 [VStaged, VScalar, EStaged, EScalar, R, R, NP, NP, TStaged, TScalar]

  val Debug = false

  /**
   * mapSigmaSPLBaseSymToCIRSym contains the mapping from all symbols from SigmaSPLBase to the CIR symbols. This is
   * required for translation from one DSL to the other, especially in the case when primitives correspond to the both
   * DSLs. For example, the iterator from sigma / SigmaSum is actually an integer, which is the same symbol in both
   * DSLs (the C-for loop iterator). Note that during the translation, symbol indices do not match. This comes from the
   * fact that the CIR symbols will receive their indices as soon as those are created.
   */
  var mapSigmaSPLBaseSymToCIRSym = HashMap.empty[Exp[Any], () => C.Exp[Any]]

  var bindMap = HashMap.empty[Exp[Any], Exp[Any]]
  var im2fmap = HashMap.empty[Exp[Any], C.Rep[Int] => C.Rep[Int]]
  var cVectorsMap = HashMap.empty[DSLType, Either[CVector[VStaged, EStaged, R, NP, TStaged], CVector[VScalar, EScalar, R, NP, TScalar]]]

  def emitIM(idx: Rep[IndexMapping]) : ((C.Rep[Int]) => C.Rep[Int]) = im2fmap.get(idx).get

  /**
   * Roll is a tag, and therefore when allocating a CVector we can safely go through it, by backtracking
   *
   * @param s
   * @return
   */
  def backtrackRolls (s: Exp[DSLType]): Vector = s match {
    case sym: Sym[_] => findDefinition(sym) match {
      case Some(stm) => stm match {
        case TP(_, TagEnd(x, Unroll())) => backtrackRolls(x)
        case _ => getDSLVector(s)
      }
      case None => getDSLVector(s)
    }
    case _ => getDSLVector(s)
  }

  def getDSLType(su : Exp[Any]): Vector = {
    val s = su.asInstanceOf[Exp[DSLType]]
    if ( bindMap.contains(s) ) {
      getDSLType(bindMap(s))
    } else su match {
      case exp: Sym[_] => findDefinition(exp) match {
        case Some(stm) => stm match {
          case TP(_, TagEnd(x, Unroll())) => backtrackRolls(x)
          case _ => getDSLVector(s)
        }
        case None => getDSLVector(s)
      }
      case _ => getDSLVector(s)
    }
  }

  /**
   * When an CIR expression is being defined for a given symbol s, add it to the map (s -> exp).
   *
   * @param s   SigmaSPL symbol.
   * @param exp CIR expression that corresponds to the given symbol.
   * @tparam T  The type of the symbol.
   */
  def setCExp [T:Manifest](s: Sym[T], exp: () => C.Exp[T]) = {
    mapSigmaSPLBaseSymToCIRSym += (s -> exp.asInstanceOf[() => C.Exp[Any]])
  }

  /**
   * For a given SigmaSPL symbol / constant, return the corresponding symbol / constant in CIR. Not that the symbols
   * will be extracted from the mapSigmaSPLBaseSymToCIRSym and as result, symbol indices may not correspond.
   *
   * @param exp sigma SPL symbol / constant.
   * @tparam T  The symbol / constant type.
   * @return    Corresponding CIR symbol / constant
   */
  def getCExp[T: Manifest](exp: Exp[T]): C.Exp[T] = exp match {
    case s@Sym(_) => {
      if (mapSigmaSPLBaseSymToCIRSym.contains(s)) {
        val cSym = mapSigmaSPLBaseSymToCIRSym(s)
        cSym ().asInstanceOf[C.Exp[T]]
      } else {
        val cSym = C.fresh[T]
        mapSigmaSPLBaseSymToCIRSym += (s -> (() => cSym))
        cSym
      }
    }
    case Const(x) => C.Const(x)
  }

  def getCVectorEither(su: Exp[Any]) = {
    val v = getDSLType(su)
    if ( !cVectorsMap.contains(v) ) assert(false, "CVector not found" + v)
    cVectorsMap(v)
  }

  def allocateCVector(su: Exp[Any]): Unit = {
    val s_dslType = getDSLType(su)
    val size = s_dslType.size
    if (!cVectorsMap.contains(s_dslType)) {
      if ( ifUnroll(s_dslType.getRep) )
        cVectorsMap += (s_dslType -> Right(DTF.createScalar(size)))
      else
        cVectorsMap += (s_dslType -> Left(DTF.createStaged(size)))
    }
  }
  def allocateCVector(sList: List[Exp[Any]]): Unit = sList.map(s => allocateCVector(s))


  def translate(stm: Stm): Unit = stm match {
    case TP(sym, Dot(a, b)) => {
      super.traverseStm(stm)
      new MatchCVector3 {
        def f[A[_], B[_], C[_], D[_], E, F[_], G[_], H[_], I[_], J, K[_], L[_], M[_], N[_], O](y: CVector[A, B, C, D, E], a: CVector[F, G, H, I, J], b: CVector[K, L, M, N, O]) : Unit = {
          import C._
          val erep_b2a = a.erep.mix(b.erep)
          val erep_a2y = y.erep.mix(a.erep)
          forloop(Const(getDSLType(sym).size), i => {
            val t1 = a(a.vrep(i))
            val t2 = b(b.vrep(i))
            y.update(y.vrep(i), erep_a2y.convert(erep_b2a.times(t1, t2)))
          }, y.vrep.staged() && a.vrep.staged() && b.vrep.staged())
        }
      }.apply(getCVectorEither(sym), getCVectorEither(a), getCVectorEither(b))
    }
    case TP(sym, Sum(a, b)) => {
      super.traverseStm(stm)
      new MatchCVector3 {
        def f[A[_], B[_], C[_], D[_], E, F[_], G[_], H[_], I[_], J, K[_], L[_], M[_], N[_], O](y: CVector[A, B, C, D, E], a: CVector[F, G, H, I, J], b: CVector[K, L, M, N, O]) : Unit = {
          import C._
          val erep_b2a = a.erep.mix(b.erep)
          val erep_a2y = y.erep.mix(a.erep)
          if (Debug) comment("Sum begins: " + stm.toString)
          forloop(Const(getDSLType(sym).size), i => {
            val t1 = a(a.vrep(i))
            val t2 = b(b.vrep(i))
            y.update(y.vrep(i), erep_a2y.convert(erep_b2a.plus(t1, t2)))
          }, y.vrep.staged() && a.vrep.staged() && b.vrep.staged())
          if (Debug) comment("Sum ends")
        }
      }.apply(getCVectorEither(sym), getCVectorEither(a), getCVectorEither(b))
    }
    case TP(sym, SigmaSum(start, end, i, body)) => {
      val size = getDSLType(sym).size
      // Vector initializer with zero
      object initToZero extends MatchCVector1 {
        def f[A[_], B[_], C[_], D[_], E](v: CVector[A, B, C, D, E]) : Unit = {
          import C._
          forloop ( Const(size), (i:Rep[Int]) => {
            v.update(v.vrep(i), v.erep.create(v.nrep.fromDouble(0), v.nrep.fromDouble(0)))
          }, v.vrep.staged())
        }
      }
      C.comment("SigmaSum starts: " + sym.toString)
      // Initialize the output space with zero
      initToZero(getCVectorEither(sym))
      // Start the SigmaSum loop
      C.for_loop(getCExp(start), getCExp(end), getCExp(i).asInstanceOf[C.Sym[Int]], tmp => {
        import C._
        // Initialize the body with zeros
        initToZero(getCVectorEither(getBlockResult(body)))
        // Emit the body of the SigmaSum
        super.traverseStm(stm)
        // Accumulate the result into the output of the sigma
        val acc = new MatchCVector2 {
          def f[A[_], B[_], C[_], D[_], E, F[_], G[_], H[_], I[_], J](y: CVector[A, B, C, D, E], x: CVector[F, G, H, I, J]) : Unit = {
            import C._
            val erepx = y.erep.mix(x.erep)
            forloop(Const(size), it => {
              val t1 = y(y.vrep(it))
              val t2 = x(x.vrep(it))
              y.update(y.vrep(it), erepx.plus(t1, t2))
            }, x.vrep.staged() && y.vrep.staged())
          }
        }
        acc(getCVectorEither(sym), getCVectorEither(getBlockResult(body)))
      })
      C.comment("SigmaSum ends: " + sym.toString)
    }
    case TP(sym, Sigma(start, end, i, body)) => {
      C.for_loop(getCExp(start), getCExp(end), getCExp(i).asInstanceOf[C.Sym[Int]], tmp => {
        C.unitToRepUnit(super.traverseStm(stm))
      })
    }
    case TP(s, Gather(im, in)) => {
      super.traverseStm(stm)
      new MatchCVector2 {
        def f[A[_], B[_], C[_], D[_], E, F[_], G[_], H[_], I[_], J](x: CVector[A, B, C, D, E], y: CVector[F, G, H, I, J]) : Unit = {
          import C._
          val erepx = y.erep.mix(x.erep)
          if (Debug) comment("Gather starts: " + s)
          forloop(Const(getDSLType(s).size), i => {
            val inew = emitIM(im)(i)
            y.update(y.vrep(i), erepx.convert(x.apply(x.vrep(inew))))
          }, x.vrep.staged() && y.vrep.staged())
          if (Debug) comment("Gather ends: " + s)
        }
        override def apply (a: EitherX, b: EitherX) = (a, b) match {
          case (Right(x), Left(y))  => assert(false, " this should not happen - (gather from scalar into array)")
          case _ => super.apply(a, b)
        }
      }.apply(getCVectorEither(in), getCVectorEither(s))
    }
    case TP(s, Scatter(im, in)) => {
      super.traverseStm(stm)
      new MatchCVector2 {
        def f[A[_], B[_], C[_], D[_], E, F[_], G[_], H[_], I[_], J](x: CVector[A, B, C, D, E], y: CVector[F, G, H, I, J]) : Unit = {
          import C._
          val erepx = y.erep.mix(x.erep)
          if (Debug) comment("Scatter starts: " + s)
          forloop (Const(getDSLType(in).size), i => {
            val inew = emitIM(im)(i)
            val t1 = y.vrep(inew)
            val t2 = x.vrep(i)
            y.update(t1, erepx.convert(x.apply(t2)))
          }, x.vrep.staged() && y.vrep.staged())
        }
        override def apply (a: EitherX, b: EitherX) = (a, b) match {
          case (Left(x), Right(y))  => assert(false, " this should not happen - (scatter from  array into scalar)")
          case _ => super.apply(a, b)
        }
      }.apply(getCVectorEither(in), getCVectorEither(s))
    }
    case TP(s_im, IM_H(b, s, _, _)) => { super.traverseStm(stm); im2fmap += (s_im -> ((i: C.Rep[Int]) => {
      import C._;
      val bc = getCExp(b)

      val sc =  getCExp(s)

      val t1 = i * sc
      val t2 = bc + t1
/*      println("HIM!!!!")
      println("Sigma: " + b)
      println("C: " + bc)
      println("Sigma: " + s)
      println("C: " + sc)
      println(t1)
      println(t2)*/

      t2
    })) }
    case TP(s_im, IM_L(k, m, _, _)) => { super.traverseStm(stm); im2fmap += (s_im -> ((i: C.Rep[Int]) => i)) }
    case TP(s_im, IM_Compose (a1, b1, _, _)) => { super.traverseStm(stm); im2fmap += (s_im -> ((i: C.Rep[Int]) => {
      val xf = im2fmap.get(a1).get
      val yf = im2fmap.get(b1).get
      xf(yf(i))
    })) }

    case TP(s, e@NumericPlus  (a, b)) => super.traverseStm(stm); val v =()=> C.numeric_plus   (getCExp(a), getCExp(b))(e.aev, e.mev, mpos(s.pos)); setCExp(s, v);
    case TP(s, e@NumericMinus (a, b)) => super.traverseStm(stm); val v =()=> C.numeric_minus  (getCExp(a), getCExp(b))(e.aev, e.mev, mpos(s.pos)); setCExp(s, v);
    case TP(s, e@NumericTimes (a, b)) => super.traverseStm(stm); val v =()=> C.numeric_times  (getCExp(a), getCExp(b))(e.aev, e.mev, mpos(s.pos)); setCExp(s, v);
    case TP(s, e@NumericDivide(a, b)) => super.traverseStm(stm); val v =()=> C.numeric_divide (getCExp(a), getCExp(b))(e.aev, e.mev, mpos(s.pos)); setCExp(s, v);

    case TP(s, e@IntTimes (a, b)) => super.traverseStm(stm); val v =()=> C.int_times  (getCExp(a), getCExp(b)); setCExp(s, v);
    case TP(s, e@IntPlus  (a, b)) => super.traverseStm(stm); val v =()=> C.int_plus   (getCExp(a), getCExp(b)); setCExp(s, v);
    case TP(s, e@IntMinus (a, b)) => super.traverseStm(stm); val v =()=> C.int_minus  (getCExp(a), getCExp(b)); setCExp(s, v);
    case TP(s, e@IntDivide(a, b)) => super.traverseStm(stm); val v =()=> C.int_divide (getCExp(a), getCExp(b)); setCExp(s, v);
    case TP(s, e@Pow(a, b))       => super.traverseStm(stm); val v =()=> C.infix_pow  (getCExp(a), getCExp(b)); setCExp(s, v);

    case TP(s, e@Pow(lhs, rhs)) => { super.traverseStm(stm); val v = () => C.infix_pow(getCExp(lhs), getCExp(rhs)); setCExp(s, v) }
    case _ => {
      //println("Seems we are missing :", stm)
      super.traverseStm(stm)
    }
  }

  def preInit       () : Unit = {}
  def postInit      () : Unit = {}
  def preBind       () : Unit = {}
  def postBind      () : Unit = {}
  def preAllocate   () : Unit = {}
  def postAllocate  () : Unit = {}
  def preTranslate  () : Unit = {}
  def postTranslate () : Unit = {}

  def transform(in: List[Any], b1: Block[DSLType]) = {
    val inputs = in.asInstanceOf[List[DSLType]]
    val block = preUnroll(b1)
    //val block =b1
    val t = getDSLTypeInstances(getBlockResult(block.asInstanceOf[Block[DSLType]]))
    val wrapped_f: (List[VStaged[Array[TStaged]]] => C.Rep[Unit]) = (in: List[VStaged[Array[TStaged]]]) => {
      preInit(); initArguments(inputs, t, in); postInit();
      traversalMode = Bind
      preBind(); this.traverseBlock(block); postBind();
      traversalMode = Allocate
      preAllocate(); this.traverseBlock(block); postAllocate();
      traversalMode = Translate
      preTranslate(); C.unitToRepUnit(this.traverseBlock(block))
    }
    val mList = inputs.map(_ =>  manifest[Array[TStaged]]) :+ manifest[Array[TStaged]]
    (wrapped_f.asInstanceOf[List[C.Exp[Any]] => C.Exp[Unit]], mList.asInstanceOf[List[Manifest[Any]]])
  }

  def initArguments(inputs: List[DSLType], t: DSLType, in: List[VStaged[Array[TStaged]]]): Unit = {
    ((inputs :+ t) zip in).foreach(m => {
      val (dslType, cRep) = m
      val v = dslType.asInstanceOf[IR.Vector]
      val cmRep = C.reflectMutableSym(cRep.asInstanceOf[C.Sym[Array[TStaged]]]).asInstanceOf[VStaged[Array[TStaged]]]
      val cvec = DTF.createStaged(v.size,cmRep)
      if (Debug) println(" !! Creating CVec of size " + v.size + " for " + dslType)
      cVectorsMap += (dslType -> Left(cvec))
    })
  }


  def bind (source: Exp[Any], sink: Exp[Any]) : Unit = {
    bindMap += (source -> (sink))
  }

  def bind(stm: Stm): Unit = {
    stm match {
      case TP(s, TagEnd(x, Unroll()))   => bind(x, s)
      case TP(s, TagStart(x, Unroll())) => bind(x, s)
      case TP(s, Sigma(_, _, _, b))     => bind(getBlockResult(b), s)
      case TP(s, DirectSum(x,y))        => { bind(x, s); bind(y, s); }
      case _ =>
    }
    super.traverseStm(stm)
  }

  def preAllocate(stm: Stm): Unit = {
    stm match {
      case TP(s, TagEnd(x, Unroll()))   => allocateCVector(List(s, x))
      case TP(s, TagStart(x, Unroll())) => allocateCVector(List(s, x))
      case TP(s, Sum(x, y))             => allocateCVector(List(s, x, y))
      case TP(s, DirectSum(x, y))       => allocateCVector(List(s, x, y))
      case TP(s, Sigma(_, _, _, b))     => allocateCVector(List(s, getBlockResult(b)))
      case TP(s, SigmaSum(_, _, _, b))  => allocateCVector(List(s, getBlockResult(b)))
      case TP(s, Gather(_, x))          => allocateCVector(List(s, x))
      case TP(s, Scatter(_, x))         => allocateCVector(List(s, x))
      case _ =>
    }
    super.traverseStm(stm)
  }

  def preUnroll (block: Block[DSLType]) = {
    var (foundSigma, b) = (false, block)
    do {
      // restart the unrolling annotation
      unrolledExp = Set.empty[Exp[Any]]
      traversalMode = Unrolling
      traverseBlock(b)
    } while (foundSigma)
    b
  }

  var unrolledExp = Set.empty[Exp[Any]]
  def ifUnroll(exp: Exp[Any]) = unrolledExp.contains(exp)

  def unrolling (stm: Stm): Unit = stm match {
    case TP(s, TagEnd(body, Unroll()))       => super.traverseStm(stm)
    case TP(s, TagStart(body, Unroll()))     => unrolledExp += body; super.traverseStm(stm)
    case TP(s, d) if ifUnroll(s) => syms(d).foreach(x => unrolledExp += x); super.traverseStm(stm)
    case _ => super.traverseStm(stm)
  }

  override def traverseStm(stm: Stm): Unit = traversalMode match {
    case Bind => bind(stm)
    case Allocate => preAllocate(stm)
    case Translate => translate(stm)
    case Unrolling => unrolling(stm)
    case _ => super.traverseStm(stm)
  }

  override def traverseStmsInBlock[A](stms: List[Stm]): Unit = traversalMode match {
    case Unrolling =>  stms.reverse foreach traverseStm
    case _ => stms foreach traverseStm
  }

  ///////////////////////////////////////////////////////////////
  /////////         THIS MUST BE REMOVED SOON           /////////
  ///////////////////////////////////////////////////////////////

  def getCVector(su: Rep[Any], c : Int = 0): (CVector[VStaged, EStaged, R, NP, TStaged], CVector[VScalar, EScalar, R, NP, TScalar]) = {
    getCVectorEither(su) match {
      case Left(v)  => (v, null)
      case Right(v) => (null, v)
    }
  }

  ///////////////////////////////////////////////////////////////

}
