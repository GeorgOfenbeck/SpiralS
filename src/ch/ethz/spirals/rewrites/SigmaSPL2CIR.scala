/**
 *     _______  _______  ___   __      ____     Automatic
 *    / __/ _ \/  _/ _ \/ _ | / /     / __/     * Implementation
 *   _\ \/ ___// // , _/ __ |/ /__   _\ \       * Optimization
 *  /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation
 *                                              of DSP Algorithms
 *  https://bitbucket.org/GeorgOfenbeck/spirals
 *  SpiralS 0.1 Prototype - ETH Zurich
 *  Copyright (C) 2013  Georg Ofenbeck (ofenbeck@inf.ethz.ch)
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

import ch.ethz.spirals.datatypes._

abstract class SigmaSPL2CIR [V1[_], V2[_], E1[_], E2[_], R[_], NP[_], T1, T2]
  (DTF: DataTypeFactory[V1, V2, E1, E2, R, NP, T1, T2])(implicit mAT: Manifest[Array[T1]])
    extends SigmaSPLBase2CIR[V1, V2, E1, E2, R, NP, T1, T2](DTF) {

  val IR: ch.ethz.spirals.dsls.SigmaSPL_DSL
  import IR._

  val biggestT: Int

  var TwiddleIndex: C.Rep[Array[Int]] = null
  var TwiddleList_re : List[Double] = List()
  var TwiddleList_im : List[Double] = List()

  var TwiddleArray_re: C.Rep[Array[Double]]  = null
  var TwiddleArray_im: C.Rep[Array[Double]]  = null

  /*

  def scalarF2(s: Sym[Any], in: Exp[Any],  n: Int): Unit = {
    val x = getCVector(in)
    val y = getCVector(s.asInstanceOf[Rep[DSLType]], true)
    C.comment("start F2")
    val idx0 = vrep(0)
    val idx1 = vrep(1)
    val t1 = x(idx0)
    val t2 = x(idx1)
    val r1 = erep.plus(t1,t2)
    val r2 = erep.minus(t1,t2)
    y.update(idx0,r1)
    y.update(idx1,r2)
    C.comment("end F2")
  }




               */
  def fL[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx], size: Int,n :Int, d: Int): Unit = {
  //def scalarL(s: Sym[Any], in: Exp[Any], size: Int,n :Int, d: Int): Unit = {

    val vrepx = y.vrep
    val erepx = y.erep
    val nrepx = y.nrep

    C.comment("start L")
    /*C.forloop(size, (i_it: C.Rep[Int]) => {
      val i = yIM(i_it).asInstanceOf[V[Int]]
      y.update(i,x.apply(i))
    })*/
    for (i <- 0 until size)
    {
      val idx = vrepx(i)
      val m = n/d
      val newindex = i/m + d* (i%m)
      val idx2 = vrepx(newindex)
      y(idx) = x(idx2)
    }
    C.comment("end L")
  }


  def fW[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx],  size: Int, n: Int, phi : Int, g: Int): Unit = {

    val vrepx = y.vrep
    val erepx = y.erep
    val nrepx = y.nrep

    def transpose (order: List[Int]): List[Int] =
    {
      val t_array = new Array[Int](order.size)
      for (i <- 0 until order.size) {
        val pos = order(i)
        t_array(pos) = i }
      t_array.toList
    }
    C.comment("start W")
    for (i <- 0 until n)
    {
      val idx = vrepx((i))
      val t = (1 until n).toList map (i => (phi * ((BigInt(g).pow(i-1)) %n).toInt))
      val order = transpose(0::t)
      val idx2 = vrepx(order(i))
      y(idx2) = x(idx)
    }
    C.comment("end W")
  }

  def fWt[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx],size: Int, n: Int, phi : Int, g: Int) = {

    val vrepx = y.vrep
    val erepx = y.erep
    val nrepx = y.nrep

    C.comment("start Wt")
    for (i <- 0 until n)
    {
      val idx = vrepx((i))
      val t = (1 until n).toList map (i => (phi * ((BigInt(g).pow(i-1)) %n).toInt))
      val order = 0::t
      val idx2 = vrepx(order(i))
      y(idx2) = x(idx)
    }
    C.comment("end Wt")
  }

  def fV[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx],  size: Int, r: Int, s : Int, alpha: Int, beta: Int) = {

    val vrepx = y.vrep
    val erepx = y.erep
    val nrepx = y.nrep

    C.comment("start V")
    for (i <- 0 until size)
    {
      val idx = vrepx((i))
      val order = ch.ethz.spirals.util.MathUtilities.CRT(r,s,alpha,beta)
      val idx2 = vrepx(order(i))
      y(idx2) = x(idx)
    }
    C.comment("end V")
  }

  def fVt[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx],  size: Int, r: Int, s : Int, alpha: Int, beta: Int) = {

    val vrepx = y.vrep
    val erepx = y.erep
    val nrepx = y.nrep

    C.comment("start Vt")
    for (i <- 0 until size)
    {
      val idx = vrepx((i))
      val order = ch.ethz.spirals.util.MathUtilities.CRT_transpose(r,s,alpha,beta)
      val idx2 = vrepx(order(i))
      y(idx2) = x(idx)
    }
    C.comment("end Vt")
  }

  def donothing[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx], size: Int) = {
    C.comment("start nothing " + size)
    for (i <- 0 until size)
    {
      val idx = y.vrep(i)
      y(idx) = x(idx)
    }
    C.comment("end nothing")
  }


  def fF2[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx], size: Int) {

    val vrepx = y.vrep
    val erepx = y.erep

    C.comment("start F2")
    val idx0 = vrepx(0)
    val idx1 = vrepx(1)
    val t1 = x(idx0)
    val t2 = x(idx1)
    val r1 = erepx.plus(t1,t2)
    val r2 = erepx.minus(t1,t2)
    y.update(idx0,r1)
    y.update(idx1,r2)
    C.comment("end F2")
  }



  def fRaderMid[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx], size: Int, n: Int): Unit = {

    val vrepx = y.vrep
    val erepx = y.erep
    val nrepx = y.nrep

    C.comment("start Rader Mid")
    val idx0 = vrepx(0)
    val idx1 = vrepx(1)

    val t1 = x(idx0)
    val t2 = x(idx1)

    val re = nrepx.fromDouble((1/(n-1.0)))
    //val re = nrep.fromDouble(0 )
    val im = nrepx.fromDouble(0)
    val tele = erepx.create(re,im)
    val t3 = erepx.times(t2,tele)

    val r1 = erepx.plus(t1,t2)
    val r2 = erepx.minus(t1,t3)
    y.update(idx0,r1)
    y.update(idx1,r2)
    //})
    C.comment("end Rader Mid")
  }


  def fRaderDiag[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx], size: Int, n: Int, k: Int, root: Int): Unit = {

    val vrepx = y.vrep
    val erepx = y.erep
    val nrepx = y.nrep

    C.comment("start Rader Diag")
    import ch.ethz.spirals.util._
    import org.apache.commons.math3.linear._
    import org.apache.commons.math3.complex.{ComplexFormat, ComplexField, Complex}
    import org.apache.commons.math3.transform.{TransformType, DftNormalization, FastFourierTransformer}

    val transformer = new FastFourierTransformer(DftNormalization.UNITARY)
    val tolerance = 1E-12
    import ch.ethz.spirals.util._
    val p = (0 until n-1).toList map (index => ( MathUtilities.mathmod((k * BigInt(root).pow(index) ), n) ))
    //val x = (0 until n-1).toList map (index => Utilities.firstrootOfUnity(n,Utilities.mathmod((k * BigInt(root).pow(index) ), n) ))

    //val x = (0 until n-1).toList map (index => Utilities.firstrootOfUnity(n,Utilities.mathmod((k * BigInt(root).pow(index) ), n) ))
//    implicit object DoubleX1 extends C.NumericRep.DoubleX

    // import C.NumericOps._
    val t: C.E[Double] = C.E[Double](n)(C.ImplicitOps.DoubleNoRep)
    val x_re = (0 until n-1).toList map (index =>  t.re(MathUtilities.mathmod((k * BigInt(root).pow(index) ), n) ))
    val x_im = (0 until n-1).toList map (index =>  t.im(MathUtilities.mathmod((k * BigInt(root).pow(index) ), n) ))
    var tarray = new Array[Double](x_re.size*2)
    if (Debug) println("--------!!!______________", n)
    for (i <- 0 until x_re.size) {
      tarray(i*2) = x_re(i)
      tarray(i*2+1) = -x_im(i)
      //tarray(i*2+1) = -tarray(i*2+1)

      if (Debug) println(x_re(i))
      if (Debug) println(x_im(i))
    }


    import ch.ethz.spirals.conf._
    if ( Config().validationconfig.useFFTW ) {
      tarray = ch.ethz.spirals.validation.fftw3j_1D.fftw3j_1D(tarray, x_re.size)
    } else {
      import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D
      println("WARNING!!!!!! Using JTransform for Radar Mid! This will result in suboptimal code and only recommended for development. (will yield non zero values where they should be zero)")
      val fftj = new DoubleFFT_1D(x_re.size)
      fftj.complexForward(tarray)
    }



    for (i <- 2 until n)
    {
      val t_complex = new Complex(tarray((i-1)*2),-tarray((i-1)*2+1))

      val d_complex = t_complex.divide(n-1)
      val re = nrepx.fromDouble(d_complex.getReal())
      val im = nrepx.fromDouble(d_complex.getImaginary())
      if (Debug) println("--------!!!______________")

      if (Debug) println(t_complex.getReal())
      if (Debug) println(t_complex.getImaginary())
      if (Debug) println(re)
      if (Debug) println(im)

      //val re:T = nrepx.fromDouble(99)
      //val im:T = nrepx.fromDouble(99)
      val t = erepx.create(re,im)
      val idx = vrepx(i-2)
      val res = erepx.times(x(idx), t)       //FIXME
      //println("!!!! rader")
      y.update(idx,res)
    }
    C.comment("end Rader Diag")
  }


  def fT[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx], size: Int, n: Int, d: Int, k: Int): Unit = {

    val vrepx = y.vrep
    val erepx = y.erep
    val nrepx = y.nrep

    C.comment("start T")
    import ch.ethz.spirals.util._

    val diag  = MathUtilities.diagTensor(MathUtilities.dLin(n/d,1,0), MathUtilities.dLin(d,1,0))
    val t = C.E[Rx[Px[Tx]]](n)(nrepx)
    val root_list_re = diag map ( ele => t.re(ele.toInt*k))
    val root_list_im = diag map ( ele => t.im(ele.toInt*k))
    for (i <- 0 until root_list_re.size)
    {
      val xi = vrepx(i)
      val u = erepx.create(root_list_re(i),root_list_im(i))
      //val idx = vrep(yi)
      val res = erepx.times(x(xi), u)
      y.update(xi,res)
    }
    C.comment("end T")
  }

  def fT3L[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx], size: Int, n: Int, d: Int, k: Int): Unit = {

    val vrepx = y.vrep
    val erepx = y.erep
    val nrepx = y.nrep

    C.comment("start T3L")
    import ch.ethz.spirals.util._

    val diag  = MathUtilities.diagTensor(MathUtilities.dLin(n/d,1,0), MathUtilities.dLin(d,2,1))
    val t = C.E[Rx[Px[Tx]]](2*n)(nrepx)
    val clist_re = diag map ( ele => t.re(ele.toInt*k))
    val clist_im = diag map ( ele => t.im(ele.toInt*k))
    val root_list_re = clist_re.grouped(2).toList.transpose.flatten //this is the L(n/2,k) part
    val root_list_im = clist_im.grouped(2).toList.transpose.flatten
    for (i <- 0 until root_list_re.size)
    {
      val xi = vrepx(i)
      val u = erepx.create(root_list_re(i),root_list_im(i))
      //val idx = vrep(yi)
      val res = erepx.times(x(xi), u)
      y.update(xi,res)
    }
    C.comment("end T3L")



  }

  def fS_T3L[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx], size: Int, n: Int, d: Int, k: Int, im: Rep[IndexMapping]): Unit = {

    val vrepx = y.vrep
    val erepx = y.erep

    C.comment("start S_T3L")
    for (i <- 0 until size)
    {
      val xi = vrepx(i)
      val IM = emitIM(im)
      val tidx = IM(C.Const(i)).asInstanceOf[C.Rep[Int]]

      val t_re :Rx[Px[Tx]] = C.array_apply(TwiddleArray_re,tidx).asInstanceOf[Rx[Px[Tx]]]
      val t_im :Rx[Px[Tx]] = C.array_apply(TwiddleArray_im,tidx).asInstanceOf[Rx[Px[Tx]]]
      val u = erepx.create(t_re,t_im)

      val res = erepx.times(x(xi), u)
      y.update(xi,res)
    }
    C.comment("end S_T3L")
  }



  def fS_RaderDiag[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx], size: Int, n: Int, k: Int, root: Int, im: Rep[IndexMapping]): Unit = {

    val vrepx = y.vrep
    val erepx = y.erep

    val Debug = false
    C.comment("start S_RaderDiag")
    if (Debug) println("S_RaderDiag")

    for (i <- 0 until size)
    {
      val xi = vrepx(i)
      val IM = emitIM(im)
      val tidx = IM(C.Const(i)).asInstanceOf[C.Rep[Int]]

      if (Debug) println("....")
      if (Debug) println(tidx)
      if (Debug) println("....")
      //C.globalDefs map println
      tidx match {
        case C.Const(l:Int) =>
        {
          val index =
          {
            import C._
            TwiddleIndex(0)
          }
          val re1 = C.array_apply(TwiddleArray_re,index)
          val im1 = C.array_apply(TwiddleArray_im,index)
          val something =   {
            import C._
            TwiddleIndex(0) = index + 1
          }
          val t_re :Rx[Px[Tx]] = re1.asInstanceOf[Rx[Px[Tx]]]
          val t_im :Rx[Px[Tx]] = im1.asInstanceOf[Rx[Px[Tx]]]
          val u = erepx.create(t_re,t_im)

          val res = erepx.times(x(xi), u)
          y.update(xi,res)
        }
        case _ =>
        {
          val index =
          {
            import C._
            TwiddleIndex(0)
          }
          val re1 = C.array_apply(TwiddleArray_re,index)
          val im1 = C.array_apply(TwiddleArray_im,index)
          val something =   {
            import C._
            TwiddleIndex(0) = index + 1
          }
          val t_re :Rx[Px[Tx]] = re1.asInstanceOf[Rx[Px[Tx]]]
          val t_im :Rx[Px[Tx]] = im1.asInstanceOf[Rx[Px[Tx]]]
          val u = erepx.create(t_re,t_im)

          val res = erepx.times(x(xi), u)
          y.update(xi,res)
        }
      }

    }
    C.comment("end S_RaderDiag")
  }


  def fS_T[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx], size: Int, n: Int, d: Int, k: Int, im: Rep[IndexMapping]): Unit = {

    val vrepx = y.vrep
    val erepx = y.erep
    val nrepx = y.nrep

    val Debug = false
    C.comment("start S_T")
    if (Debug) println("S_T")

    for (i <- 0 until size)
    {
      val xi = vrepx(i)
      val IM = emitIM(im)
      val tidx = IM(C.Const(i)).asInstanceOf[C.Rep[Int]]

      if (Debug) println("....")
      if (Debug) println(tidx)
      if (Debug) println("....")
      //C.globalDefs map println
      tidx match {
        case C.Const(l:Int) =>
        {
          import ch.ethz.spirals.util._

          //val t = C.E[Tx](n) //check if this is correct
          val t = ch.ethz.spirals.util.Registry.getTwidd(n: Int)
          //val diag  = Utilities.diagTensor(Utilities.dLin(n/d,1,0), Utilities.dLin(d,1,0))
          val diag  = ch.ethz.spirals.util.Registry.getDiagTensor(n,d)
          val diagv = diag(l)
          val re = t.re(diagv.toInt*k)
          val im = t.im(diagv.toInt*k)

          val t_re :Rx[Px[Tx]] = y.nrep.fromDouble(re)
          val t_im :Rx[Px[Tx]] = y.nrep.fromDouble(im)
          val u = erepx.create(t_re,t_im)


          //this is just here while we carry those numbers in the array as well...
          val index =
          {
            import C._
            TwiddleIndex(0)
          }

          val something =   {
            import C._
            TwiddleIndex(0) = index + 1
          }


          val res = erepx.times(x(xi), u)
          y.update(xi,res)
        }
        case _ =>
        {

          /*
          val (re1,im1) = {
            import C._

            val re1 = C.array_apply(TwiddleArray_re,TwiddleIndex)
            val im1 = C.array_apply(TwiddleArray_im,TwiddleIndex)
            TwiddleIndex = TwiddleIndex + 1
            (re1,im1)
          }
          /* val index = C.array_apply[Int](TwiddleIndex,C.Const(0))

           val indexplus = C.numeric_plus(index,C.Const(1))
           C.array_update[Int](TwiddleIndex,C.Const(0),indexplus)
         }*/
            */


          //assert(false, tidx)
          //val re1 = C.array_apply(TwiddleArray_re,tidx)
          //val im1 = C.array_apply(TwiddleArray_im,tidx)
          val index =
            {
              import C._
              TwiddleIndex(0)
            }
          val re1 = C.array_apply(TwiddleArray_re,index)
          val im1 = C.array_apply(TwiddleArray_im,index)
          val something =   {
            import C._
            TwiddleIndex(0) = index + 1
            }
          val t_re :Rx[Px[Tx]] = re1.asInstanceOf[Rx[Px[Tx]]]
          val t_im :Rx[Px[Tx]] = im1.asInstanceOf[Rx[Px[Tx]]]
          val u = erepx.create(t_re,t_im)

          val res = erepx.times(x(xi), u)
          y.update(xi,res)
        }
      }

    }
    C.comment("end T")
  }


  def fD2[Vx[_], Ex[_], Px[_], Rx[_], Tx](x: CVector[Vx, Ex, Rx, Px, Tx], y:  CVector[Vx, Ex, Rx, Px, Tx], size: Int, k: Int): Unit = {

    val vrepx = y.vrep
    val erepx = y.erep
    val nrepx = y.nrep

    C.comment("start d2")
    val t = C.E[Rx[Px[Tx]]](4)(nrepx) //check if this is correct
    val re = t.re(1*k)
    val im = t.im(1*k)
    val tele = erepx.create(re,im)

    val idx0 = vrepx(0)
    val idx1 = vrepx(1)
    val res = erepx.times(x(idx1),tele)

    y(idx0) = x(idx0)
    y.update(idx1, res)


    C.comment("end d2")
  }


  /*

   def donothing(s: Sym[Any], in: Exp[Any], size: Int): Unit = {
     val x = getCVector(in)
     val y = getCVector(s.asInstanceOf[Rep[DSLType]], true)

     C.comment("start nothing")
     for (i <- 0 until size)
     {
       val idx = vrep(i)
       y(idx) = x(idx)
     }
     C.comment("end nothing")
   } */



  val ImDebug = false
  override def translate(stm: Stm): Unit = {
    //println("looking for match for ", stm)
    if (Debug) println(stm)
    stm match {

      case TP(s, SPL_D2(Const(k:Int),in)) => {
        val xt = getCVector(in)
        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println(" start T ")
        if (ImDebug)
        {
          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)
        }
        else
        {
          if (xt._1 != null)
            fD2(xt._1,yt._1,size,k)
          else
            fD2(xt._2,yt._2, size,k)
        }
      }

      case TP(s, SPL_F2(in)) => {
        val xt = getCVector(in)
        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println(" start T ")
        if (ImDebug)
        {
        if (xt._1 != null)
           donothing(xt._1,yt._1,size)
        else
           donothing(xt._2,yt._2, size)
        }
        else
        {
          if (xt._1 != null)
          {
            //println("F2 array")
            fF2(xt._1,yt._1,size)
          }
          else
          {
            //println("F2 scalar")
            fF2(xt._2,yt._2, size)
          }
        }
      }



      case TP(s, SPL_T(Const(n:Int),Const(d:Int),Const(k:Int), in)) => {
        val xt = getCVector(in)

        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size

        if (Debug) println(" f2 ")
        if (ImDebug)
        {
          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)
        }
        else
        {
          if (xt._1 != null)
            fT(xt._1,yt._1,size,n,d,k)
          else
            fT(xt._2,yt._2, size,n,d,k)
        }
      }
      case TP(s, SPL_T3L(Const(n:Int),Const(d:Int),Const(k:Int), in)) => {
        val xt = getCVector(in)
        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println(" f2 ")
        if (ImDebug)
        {
          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)
        }
        else
        {
          if (xt._1 != null)
            fT3L(xt._1,yt._1,size,n,d,k)
          else
            fT3L(xt._2,yt._2, size,n,d,k)
        }
      }

      case TP(s, SPL_V(Const(r:Int),Const(s1:Int),Const(alpha:Int), Const(beta:Int) ,in)) => {
        val xt = getCVector(in)
        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println(" start V ")
        if (ImDebug)
        {
          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)
        }
        else
        {
          //scalarW(s,in,size,n,phi,g)
          if (xt._1 != null)
            fV(xt._1,yt._1,size,r,s1,alpha,beta)
          else
            fV(xt._2,yt._2,size,r,s1,alpha,beta)
        }
      }


      case TP(s, SPL_Vt(Const(r:Int),Const(s1:Int),Const(alpha:Int), Const(beta:Int) ,in)) => {
        val xt = getCVector(in)
        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println(" start V ")
        if (ImDebug)
        {
          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)
        }
        else
        {
          //scalarW(s,in,size,n,phi,g)
          if (xt._1 != null)
            fVt(xt._1,yt._1,size,r,s1,alpha,beta)
          else
            fVt(xt._2,yt._2,size,r,s1,alpha,beta)
        }
      }





      case TP(s, SPL_W(Const(n:Int),Const(phi:Int),Const(g:Int),in)) => {
        val xt = getCVector(in)
        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println(" start l ")
        if (ImDebug)
        {
          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)
        }
        else
        {
          //scalarW(s,in,size,n,phi,g)
          if (xt._1 != null)
            fW(xt._1,yt._1,size,n,phi,g)
          else
            fW(xt._2,yt._2,size,n,phi,g)
        }
      }


      case TP(s, SPL_Wt(Const(n:Int),Const(phi:Int),Const(g:Int),in)) => {
        val xt = getCVector(in)
        //allocateCVector2(s,xt._1 == null)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println(" start l ")
        if (ImDebug)
        {
          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)
        }
        else
        {
          //scalarWt(s,in,size,n,phi,g)
          if (xt._1 != null)
            fWt(xt._1,yt._1,size,n,phi,g)
          else
            fWt(xt._2,yt._2,size,n,phi,g)
        }
      }


      case TP(s, SPL_L(Const(n:Int),Const(d:Int),in)) => {
        val xt = getCVector(in)
        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println(" start l ")
        if (ImDebug)
        {
          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)
        }
        else
        {
          if (xt._1 != null)
            fL(xt._1,yt._1,size,n,d)
          else
            fL(xt._2,yt._2, size,n,d)
        }
      }


      case TP(s, SPL_I(Const(n),in)) => {
        val xt = getCVector(in)
        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println("In Size!! "+ size)
        if (Debug) println(" start l ")

          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)

      }


      case TP(s, SPL_RaderMid(Const(n:Int),in)) => {
        val xt = getCVector(in)
        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println(" start l ")
        if (ImDebug)
        {
          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)
        }
        else
        {
          //fRaderMid(s,in,size,n)
          if (xt._1 != null)
            fRaderMid(xt._1,yt._1,size,n)
          else
            fRaderMid(xt._2,yt._2, size,n)

        }
      }

      case TP(s, SPL_RaderDiag(Const(n:Int),Const(k:Int),Const(root:Int),in)) => {
        val xt = getCVector(in)
        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println(" start l ")
        if (ImDebug)
        {
          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)
        }
        else
        {
          // fRaderDiag(s,in,size,n,k,root)
          if (xt._1 != null)
            fRaderDiag(xt._1,yt._1,size,n,k,root)
          else
            fRaderDiag(xt._2,yt._2, size,n,k,root)
        }
      }



      case TP(s, SPL_S_T3L(Const(n:Int),Const(d:Int),Const(k:Int),f, in)) => {
        val xt = getCVector(in)
        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println(" start l ")

        if (ImDebug)
        {
          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)
        }
        else
        {
          if (xt._1 != null)
            fS_T(xt._1,yt._1,size,n,d,k,f)
          else
            fS_T(xt._2,yt._2, size,n,d,k,f)
        }
      }

      case TP(s, SPL_S_RaderDiag(Const(n:Int),Const(k:Int),Const(root:Int),f, in)) => {
        val xt = getCVector(in)
        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println(" start l ")

        if (ImDebug)
        {
          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)
        }
        else
        {
          if (xt._1 != null)
            fS_RaderDiag(xt._1,yt._1,size,n,k,root,f)
          else
            fS_RaderDiag(xt._2,yt._2, size,n,k,root,f)
        }
      }


      case TP(s, SPL_S_T(Const(n:Int),Const(d:Int),Const(k:Int),f, in)) => {
        val xt = getCVector(in)
        allocateCVector(s)
        val yt = getCVector(s.asInstanceOf[Rep[DSLType]])
        val inv = getDSLType(in).asInstanceOf[IR.Vector]
        val size = inv.size
        if (Debug) println(" start l ")

        if (ImDebug)
        {
          if (xt._1 != null)
            donothing(xt._1,yt._1,size)
          else
            donothing(xt._2,yt._2, size)
        }
        else
        {
          if (xt._1 != null)
            fS_T(xt._1,yt._1,size,n,d,k,f)
            else
            fS_T(xt._2,yt._2, size,n,d,k,f)

          /*{
            C.comment("start S_T")
            for (i <- 0 until size)
            {
              val xi = vrep(i)
              val IM = emitIM(f)
              val tidx = IM(C.Const(i)).asInstanceOf[C.Rep[Int]]

              val t_re :T = C.array_apply(TwiddleArray_re,tidx).asInstanceOf[T]
              val t_im :T = C.array_apply(TwiddleArray_im,tidx).asInstanceOf[T]
              val u = erep.newEle(t_re,t_im)

              val res = erep.multiply(xt._1(xi), u)
              yt._1.update(xi,res)
            }
            C.comment("end T")
          */

          //}

            /*C.comment("start S_T")
            for (i <- 0 until size)
            {
              val xi = vrep1(i)
              val IM = emitIM(f)
              val tidx = IM(C.Const(i)).asInstanceOf[C.Rep[Int]]

              val t_re :T = C.array_apply(TwiddleArray_re,tidx).asInstanceOf[T]
              val t_im :T = C.array_apply(TwiddleArray_im,tidx).asInstanceOf[T]
              val u = erep.newEle(t_re,t_im)

              val res = erep.multiply(xt._2(xi), u)
              yt._1.update(xi,res)
            }
            C.comment("end T")*/


        }
      }



      /*case TP(s, SPL_T(Const(n:Int),Const(d:Int),Const(k:Int), in)) => {
        val size = getDSLTypeInstances[IR.Vector](in).size
        if (ImDebug) donothing(s,in,size) else
          scalarT(s,in,size,n,d,k)
        //donothing(s,in,size)
        //super.translate(stm)
      }
      case TP(s, SPL_L(Const(n:Int),Const(d:Int),in)) => {
        val size = getDSLTypeInstances[IR.Vector](in).size
        if (ImDebug) donothing(s,in,size) else
          scalarL(s,in,size,n,d)
        //donothing(s,in,size)
        //super.translate(stm)
      }
      case TP(s, SPL_T3L(Const(n:Int),Const(d:Int),Const(k:Int), in)) => {
        val size = getDSLTypeInstances[IR.Vector](in).size
        if (ImDebug) donothing(s,in,size) else
          scalarT3L(s,in,size,n,d,k)
        //donothing(s,in,size)
        //super.translate(stm)
      }
      case TP(s, SPL_D2(Const(k:Int),in)) => {
        val size = getDSLTypeInstances[IR.Vector](in).size
        if (ImDebug) donothing(s,in,size) else
          scalarD2(s,in,size,k)
        //donothing(s,in,size)
        //super.translate(stm)
      }



      case TP(s, SPL_I(Const(n),in)) => {
        val size = getDSLTypeInstances[IR.Vector](in).size
        //  if (ImDebug) donothing(s,in,size) else
        //        scalarRaderDiag(s,in,size,n,k,root)
        donothing(s,in,size)
        //super.translate(stm)
      }

      case TP(s1, SPL_V(Const(r),Const(s),Const(alpha),Const(beta),in)) => {
        val size = getDSLTypeInstances[IR.Vector](in).size
        if (ImDebug) donothing(s1,in,size) else
          scalarV(s1,in,size,r,s,alpha,beta)
        //donothing(s,in,size)
        //super.translate(stm)
      }

      case TP(s1, SPL_Vt(Const(r),Const(s),Const(alpha),Const(beta),in)) => {
        val size = getDSLTypeInstances[IR.Vector](in).size
        if (ImDebug) donothing(s1,in,size) else
          scalarVt(s1,in,size,r,s,alpha,beta)
        //donothing(s,in,size)
        //super.translate(stm)
      }*/

      //case TP(s_im, IM_W(Const(g: Int), size)) => im2fmap += (s_im -> ((i: C.Rep[Int]) => {
      //case TP(s_im, IM_WT(Const(phi: Int),Const(g: Int), range, domain)) => im2fmap += (s_im -> ((i: C.Rep[Int]) => {
      case TP(s_im, IM_WT(phi, g, range, domain)) => im2fmap += (s_im -> ((i: C.Rep[Int]) => {
        import C._
        C.comment("start IM_WT")
        val gpow = infix_pow(getCExp(g),i)
        val gphi = numeric_times(gpow,getCExp(phi))
        val gmod = infix_mod(gphi,domain)
        C.comment("end IM_WT")
        gmod

      }))

      case TP(s_im, IM_W(Const(g: Int), range, domain)) => im2fmap += (s_im -> ((i: C.Rep[Int]) => {
        import C._
        C.comment("start IM_W")
        val gpow = infix_pow(g,numeric_minus(i,1))
        val gmod = infix_mod(gpow,domain)
        //val gmod = numeric_times(i,)
        C.comment("end IM_W " + domain)
        gmod
      }))


      case TP(s_im, IM_Z(b,s, range, domain)) => im2fmap += (s_im -> ((i: C.Rep[Int]) => {
        import C._
        C.comment("start IM_Z")
        val t1 = numeric_times(i,getCExp(s))
        val t2 = numeric_plus(t1, getCExp(b))
        val t3 = infix_mod(t2,domain)
        //val gmod = numeric_times(i,)
        C.comment("end IM_Z " + domain)
        t3
      }))


      case TP(s_im, IM_V(Const(m: Int),Const(k: Int), range, domain)) => im2fmap += (s_im -> ((i: C.Rep[Int]) => {
        import C._
        C.comment("start IM_V")

        val floor = int_divide(i,m)
        val t1 = numeric_times(m,floor)
        val bracket = infix_mod(i,m)
        val t2 = numeric_times(bracket,k)
        val t3 = numeric_plus(t1,t2)
        val mod = infix_mod(t3,m*k)
        //val gmod = numeric_times(i,)
        C.comment("end IM_V " + domain)
        mod
      }))


      case TP(s_im, IM_Twiddle (Const(d: Int), Const(n: Int), range, domain)) => im2fmap += (s_im -> ((i: C.Rep[Int]) => {
        import C._
        //val t1 = (i / (d))
        val t1 = i match
        {
          case Const(x : Int) => Const(x/d)
          case _ => i/d
        }

        val t2 = C.Const(biggestT / n)
        val t3 = t1 * t2
        val t4 = C.infix_mod(i, d)
        val t5 = t4 * t3
        t5

      }))

      case _ =>       super.translate(stm)
    }    }

  override def bind(stm: Stm): Unit = stm match {
    case TP(s, SPL_F2  (_)) => // no need to bind anything
    case TP(s, SPL_T (_,_,_, _)) => // no need to bind anything
    case TP(s, SPL_L(_, _ , _)) => // no need to bind anything
    case _ => super.bind(stm)
  }


  override def preAllocate(stm: Stm): Unit = stm match {
    case TP(s, SPL_Wt(Const(n:Int),Const(phi:Int),Const(g:Int),in)) => {
      if (Debug) println("now allocating Wt")
      allocateCVector(s)
      //allocateCVector(x)
    }
    case _ => super.preAllocate(stm)
  }


  override def preInit (): Unit = {


    if (TwiddleList_re.isEmpty)
    {

      //assert(false)
      val tTwiddleArray_re = new Array[Double](biggestT)
      val tTwiddleArray_im = new Array[Double](biggestT)
      val start = System.currentTimeMillis
      if (Debug) println("calculating twiddles")
      if (Debug) println(biggestT)
//      implicit val forE = C.DoubleX
      //val tx = C.E[Double](biggestT)(C.ImplicitOps.DoubleNoRep)
      val tx = ch.ethz.spirals.util.Registry.getTwidd(biggestT)
      for (i <- 0 until biggestT)
      {
        //val cdt = ComplexDataType.E[Double](dft_size,i)
        tTwiddleArray_re(i) = tx.re(i)
        if (Debug) println(tx.re(i))
        tTwiddleArray_im(i) = tx.im(i)
        if (Debug) println(tx.im(i))
        if (Debug) println("-----------")
      }
      if (Debug) println("finished with calculating twiddles")
      if (Debug) println("took: ", System.currentTimeMillis - start)

      val global_array_re = C.NewGlobalConstArray[Double](tTwiddleArray_re.toList)
      val global_array_im = C.NewGlobalConstArray[Double](tTwiddleArray_im.toList)



      TwiddleArray_re = global_array_re
      TwiddleArray_im = global_array_im
    }
    else
    {
      TwiddleArray_re = C.NewGlobalConstArray[Double](TwiddleList_re)
      TwiddleArray_im = C.NewGlobalConstArray[Double](TwiddleList_im)
    }
    {
      import C._
      TwiddleIndex = C.NewArray[Int](C.Const(1))
      TwiddleIndex(C.Const(0)) = 0
    }
  }


}
