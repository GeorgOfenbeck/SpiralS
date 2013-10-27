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

package ch.ethz.spirals.dsls

import virtualization.lms.common._
import reflect.SourceContext
import ch.ethz.spirals.datatypes.DSLBaseTypes


/**
 * SPL is the base type we operate on which is a always a special matrix in the context of e.g. DFTs such as
 * Stride permutation etc.
 * @param size
 */


abstract class SPL(val size: Int) {
  //SPL Specific part
  //def transform[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T];
  def toLatex(): String = "No Latex defined"
/*  def toMatrix() : BlockFieldMatrix[Complex] =
  {
    type NoRep[T] = T
    implicit def unstagedArrayOps[T:Manifest] = new ArrayOps[NoRep,T] {
      def alloc(s: Int): Array[T] = { new Array[T](s) }
      def apply(x: Array[T], i: Int): T = { x(i) }
      def update(x: Array[T], i: Int, y: T) = x.update(i,y)
    }
    val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(),size,size)
    for (i <- 0 until size)
    {
      val x = new ScalarizedVector[NoRep,Double](size)
      for (j <- 0 until size)
        if (j == i)
          x.update(i,new AComplex[Double](1,0.0))
        else
          x.update(j,new AComplex[Double](0.0,0.0))

      val outputrow = transform(x)

      for (j <- 0 until size)
      {
        val re: Double = outputrow.apply(j)._re
        val im: Double = outputrow.apply(j)._im
        m.setEntry(j,i,new Complex(re,im))
      }
    }
    m
  }

  def pM() = ch.ethz.spirals.util.Utilities.printm(toMatrix())*/
}





/**
 * Discrete Fourier Transform
 * @param n size
 * @param k TODO
 */
case class DFT(n: Int, k: Int) extends SPL(n)
{
  def this(n: Int) = this(n,1)
  override def toString = "DFT("+n+","+k+")"
  override def toLatex = if (k != 1) "\\DFT("+n+","+k+")" else "\\DFT("+n+")"
/*  override def toMatrix() = {
    val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(),n,n)
    val t_e = ch.ethz.spirals.dsls.E[Double](n)
    for (x <- 0 until n)
      for (y <- 0 until n)
        m.setEntry(x,y,new Complex(t_e.re(x*y*k),t_e.im(x*y*k)))
    m
  }
  //TODO: add by definition transform
  override def transform[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] = in */
}

case class Rader_Mid_Matrix(n1: Int) extends SPL(2)
{
  override def toLatex =  "" //done via Directsum in 2Latex
  //override def toLatex =  "\\RaderMid("+n1+")"
  override def toString = "Rader_Mid_Matrix("+ n1 + ")"

  /*override def toMatrix() = {
      val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(),2,2)
      m.setEntry(0,0, new Complex(1,0))
      m.setEntry(0,1, new Complex(1,0))
      m.setEntry(1,0, new Complex(1,0))
      m.setEntry(1,1, new Complex(-1/(n1-1.0),0))
      m
  } */
  //TODO: add by definition transform
  //override def transform[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] = in
}

case class Rader_Diag(n: Int, k: Int, root: Int) extends SPL(n-2) with DiagonalMatrix
{    
  override def toString = "E("+ n + ","+ k + ","+ root +")"
  override def toLatex =  "" //done via Directsum in 2Latex
/*  override def transform[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E], nrep: NumericRep[T]): CVector[V, E, T] = scale(in)
  def scale[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E], nrep: NumericRep[T]): CVector[V, E, T] =
  {

    val out = in.create(vrep.liftstuff(size))

    val transformer = new FastFourierTransformer(DftNormalization.UNITARY)
    val tolerance = 1E-12
    import ch.ethz.spirals.util._
    val p = (0 until n-1).toList map (index => ( Utilities.mathmod((k * BigInt(root).pow(index) ), n) ))
    //val x = (0 until n-1).toList map (index => Utilities.firstrootOfUnity(n,Utilities.mathmod((k * BigInt(root).pow(index) ), n) ))
    //val x = (0 until n-1).toList map (index => Utilities.firstrootOfUnity(n,Utilities.mathmod((k * BigInt(root).pow(index) ), n) ))
    val t = ch.ethz.spirals.dsls.E[Double](n)
    val x_re = (0 until n-1).toList map (index =>  t.re(Utilities.mathmod((k * BigInt(root).pow(index) ), n) ))
    val x_im = (0 until n-1).toList map (index =>  t.im(Utilities.mathmod((k * BigInt(root).pow(index) ), n) ))
    var tarray = new Array[Double](x_re.size*2)
    for (i <- 0 until x_re.size) {
      tarray(i*2) = x_re(i)
      tarray(i*2+1) = -x_im(i)
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

    /*val clist = (2 until n).toList map ( i => {
      val t_complex = new Complex(tarray((i-1)*2),-tarray((i-1)*2+1))
      val d_complex = t_complex.divide(n-1)

      ComplexFactory(in(0),d_complex.getReal(), d_complex.getImaginary())

    } )*/

    for (i <- 2 until n)
    {
      val t_complex = new Complex(tarray((i-1)*2),-tarray((i-1)*2+1))
      val d_complex = t_complex.divide(n-1)
      val re:T = nrep.fromDouble(d_complex.getReal())
      val im:T = nrep.fromDouble(d_complex.getImaginary())

      //val re:T = nrep.fromDouble(99)
      //val im:T = nrep.fromDouble(99)
      val t = erep.newEle(re,im)
      val idx = vrep.liftstuff(i-2)
      val res = erep.multiply(in(idx), t)       //FIXME
      //println("!!!!")
      out.update(idx,res)

    }
    out


    //(in,clist).zipped.map(_ * _)
  }     */
}



case class WHT(n: Int) extends SPL(n)
{

  override def toLatex =  "\\WHT("+n+")"
  override def toString = "WHT("+n+")";
  /*override def toMatrix() = {
    val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(),n,n)
    WHT_recurse(1,1,0,0,n,n,m)
    def WHT_recurse(m: Int, sign: Int, x: Int, y: Int, x_top: Int, y_top: Int, matrix: BlockFieldMatrix[Complex])
    {

      if (pow(2,m) == n)
      {

        matrix.setEntry(x,y,      new Complex(1 * sign,0))
        matrix.setEntry(x+1,y,    new Complex(1 * sign,0))
        matrix.setEntry(x,y+1,      new Complex(1 * sign,0))
        matrix.setEntry(x+1,y+1,  new Complex(-1 * sign,0))

      }
      else
      {
        val cx = x_top - (x_top - x)/2
        val cy = y_top - (y_top - y)/2
        WHT_recurse(m+1,sign , x      ,y      ,cx, cy, matrix)
        WHT_recurse(m+1,sign , x      ,cy,cx, y_top,   matrix)
        WHT_recurse(m+1,sign ,cx ,y      ,x_top  , cy, matrix)
        WHT_recurse(m+1,-sign,cx ,cy,x_top  , y_top  , matrix)
        //println(m+1,-sign,x_top/2 ,y_top/2,x_top  , y_top  )
      }
    }
    m
  }*/
  //override def transform[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] = in
}




/**
 * This is the basic building block ("the butterfly") of all DFT's - note the fixed size 2
 */
case class F_2() extends SPL(2){
  override def toString = "F2"
  override def toLatex = "\\DFT_{2}"
  /*override def transform[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] =
  {
    val idx0 = vrep.liftstuff(0)
    val idx1 = vrep.liftstuff(1)
    val t1 = in(idx0)
    val t2 = in(idx1)
    val r1 = erep.plus(t1,t2)
    val r2 = erep.minus(t1,t2)
    val out = in.create(vrep.liftstuff(2))
    out.update(idx0,r1)
    out.update(idx1,r2)
    out
  } */
}


/**
 * Describes properties that all StridePermutations such as "L" share
 */
trait StridePermutation extends SPL{
  //def transform[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] = permute(in)
  //def reorder[V[_], E[_], T](in: CVector[V, E, T], order: List[Int])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] = in;
  //def permute[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T];
  def transpose (order: List[Int]): List[Int] =
  {
    val t_array = new Array[Int](order.size)
    for (i <- 0 until order.size) {
      val pos = order(i)
      t_array(pos) = i }
    t_array.toList
  }
}

/**
 * Collects all common attributes of scaling matrices such as "I" and "T"
 */
trait DiagonalMatrix extends SPL{
  //def transform[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] = scale(in)
  //def scale[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T];
}





/**
 * Identity matrix - usually only used in SPL for tensors and gone during SigmaSPL
 * @param n size
 */
case class I(n: Int) extends SPL(n) with StridePermutation
{
  override def toString = "I("+n+")"
  override def toLatex = "\\one_{"+n+"}"
  //override def permute[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] = in
  //override def transform[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] = in
}

case class D2(k: Int) extends SPL(2) with DiagonalMatrix
{
  override def toString =  "SplitRadix Diag2"
  /*override def transform[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E], nrep: NumericRep[T]): CVector[V, E, T] = scale(in)
  def scale[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E], nrep: NumericRep[T]): CVector[V, E, T] =
  //{ List(in(0),in(1)* ComplexFactory(in(0), ComplexDataType.E[T](4,k).re(), ComplexDataType.E[T](4,k).im() ))} //Utilities.firstrootOfUnity(4,k)) }
  {
    val out = in.create(vrep.liftstuff(size))
    val t = ch.ethz.spirals.dsls.E[T](4) //check if this is correct
    val re = t.re(1*k)
    val im = t.im(1*k)
    val tele = erep.newEle(re,im)

    val idx0 = vrep.liftstuff(0)
    val idx1 = vrep.liftstuff(1)
    val res = erep.multiply(in(idx1),tele)

    out(idx0) = in(idx0)
    out(idx1) = res
    out
  }*/


  //def scale[T: NumericRep](in: List[AbstrComplex[T]]): List[AbstrComplex[T]] = { List(in(0),in(1)* ComplexFactory(in(0), Utilities.firstrootOfUnity(4,k).getReal, Utilities.firstrootOfUnity(4,k).getImaginary ))} //Utilities.firstrootOfUnity(4,k)) }
}



/**
 * IM_Twiddle Scaling matrix
 * @param n size
 * @param d TODO
 * @param k TODO
 */
case class T(n: Int,d: Int,k : Int) extends SPL(n) with DiagonalMatrix
{
  def this(n: Int, d: Int) = this(n,d,1)
  import ch.ethz.spirals.util._
  override def toString = "T("+ n + ")("+ d +")("+ k +")"
  override def toLatex = "\\twiddle^{"+n+"}_{"+d+"}"
  /*override def transform[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E], nrep: NumericRep[T]): CVector[V, E, T] = scale(in)
  def scale[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E], nrep: NumericRep[T]): CVector[V, E, T] =
  {
    val out = in.create(vrep.liftstuff(n))
    val diag  = Utilities.diagTensor(Utilities.dLin(n/d,1,0), Utilities.dLin(d,1,0))
    val t = ch.ethz.spirals.dsls.E[T](n)
    val root_list_re = diag map ( ele => t.re(ele.toInt*k))
    val root_list_im = diag map ( ele => t.im(ele.toInt*k))
    for (i <- 0 until root_list_re.size)
    {
      val t = erep.newEle(root_list_re(i),root_list_im(i))
      val idx = vrep.liftstuff(i)
      val res = erep.multiply(in(idx), t)
      out.update(idx,res)
    }
    out
  }*/
}


case class T3L(n: Int,d: Int,k : Int) extends SPL(n) with DiagonalMatrix //using this to map T3 L2 in Split Radix
{
  import ch.ethz.spirals.util._
  override def toString = "fcompose(T3("+ n + ")("+ d +")("+ k +")L(n/2,2)"
 /* override def transform[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E], nrep: NumericRep[T]): CVector[V, E, T] = scale(in)
  def scale[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E], nrep: NumericRep[T]): CVector[V, E, T] =
  {
    val out = in.create(vrep.liftstuff(size))
    val diag  = Utilities.diagTensor(Utilities.dLin(n/d,1,0), Utilities.dLin(d,2,1))
    val t = ch.ethz.spirals.dsls.E[T](2*n)
    val clist_re = diag map ( ele => t.re(ele.toInt*k))
    val clist_im = diag map ( ele => t.im(ele.toInt*k))

    val root_list_re = clist_re.grouped(2).toList.transpose.flatten //this is the L(n/2,k) part
    val root_list_im = clist_im.grouped(2).toList.transpose.flatten


    for (i <- 0 until root_list_re.size)
    {
      val t = erep.newEle(root_list_re(i),root_list_im(i))
      val idx = vrep.liftstuff(i)
      val res = erep.multiply(in(idx), t)
      out.update(idx,res)
    }
    out
    /*
    val out = in.create(vrep.liftstuff(size))
    val diag  = Utilities.diagTensor(Utilities.dLin(n/d,1,0), Utilities.dLin(d,2,1))
    val root_list = diag map ( ele => ComplexDataType.E[T](2*n,ele.toInt*k))
    val clist = root_list map (root => ComplexFactory(in(0),root.re(),root.im()))

    val plist = clist.grouped(2).toList.transpose.flatten //this is the L(n/2,k) part
    val t_list = (in,plist).zipped.map(_ * _)
    t_list */
  }*/
}


/**
 * TODO
 * @param n
 * @param k
 */
case class L(n: Int,k: Int) extends SPL(n) with StridePermutation
{
  override def toString = "L("+ n + ")("+ k +")"
  override def toLatex = "\\stride^{"+n+"}_{"+k+"}"
/*  override def permute[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] =
  {
    val out = in.create(vrep.liftstuff(size))
    for (i <- 0 until n)
    {
      val idx = vrep.liftstuff(i)
      val m = n/k
      val newindex = i/m + k* (i%m)
      val idx2 = vrep.liftstuff(newindex)
      out(idx) = in(idx2)
    }
    out
  }*/
}

case class V(r: Int, s: Int, alpha : Int, beta: Int) extends SPL(r*s) with StridePermutation  //Chinese remainder theorem permutation
{
  override def toString = "V("+ r + ","+ s + ","+ alpha + ","+ beta +")"
  override def toLatex = "V_{" + r + ","+ s + "}"
 /* override def permute[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] =
  {
    import ch.ethz.spirals.util._
    val out = in.create(vrep.liftstuff(size))
    for (i <- 0 until size)
    {
      val idx = vrep.liftstuff((i))
      val order = Utilities.CRT(r,s,alpha,beta)
      val idx2 = vrep.liftstuff(order(i))
      out(idx2) = in(idx)
    }
    out
  }*/
}

case class Vt(r: Int, s: Int, alpha : Int, beta: Int) extends SPL(r*s) with StridePermutation //Chinese remainder theorem permutation transposed
{
  override def toString = "Vt("+ r + ","+ s + ","+ alpha + ","+ beta +")"
  override def toLatex = "V_{" + r + ","+ s + "}^{-1}"

  /*override def permute[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] =
  {
    import ch.ethz.spirals.util._
    val out = in.create(vrep.liftstuff(size))
    for (i <- 0 until size)
    {
      val idx = vrep.liftstuff((i))
      val order = Utilities.CRT_transpose(r,s,alpha,beta)
      val idx2 = vrep.liftstuff(order(i))
      out(idx2) = in(idx)
    }
    out
  }*/
}


case class W(n: Int, phi : Int, g: Int) extends SPL(n) with StridePermutation //Rader Permutation
{
  override def toString = "W("+ n + ","+ phi + ","+ g +")"
  override def toLatex = "W_{" + n + "} "
 /* override def permute[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] =
  {
    val out = in.create(vrep.liftstuff(size))
    for (i <- 0 until n)
    {
      val idx = vrep.liftstuff((i))
      val t = (1 until n).toList map (i => (phi * ((BigInt(g).pow(i-1)) %n).toInt))
      val order = transpose(0::t)
      val idx2 = vrep.liftstuff(order(i))
      out(idx2) = in(idx)
    }
    out
  }*/

}

case class Wt(n: Int, phi : Int, g: Int) extends SPL(n) with StridePermutation //Rader Permutation
{
  override def toString = "Wt("+ n + ","+ phi + ","+ g +")"
  override def toLatex = "W^{-1}_{" + n + "} "
/*  override def permute[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] =
  {
    val out = in.create(vrep.liftstuff(size))
    for (i <- 0 until n)
    {
      val idx = vrep.liftstuff((i))
      val t = (1 until n).toList map (i => (phi * ((BigInt(g).pow(i-1)) %n).toInt))
      val order = 0::t
      val idx2 = vrep.liftstuff(order(i))
      out(idx2) = in(idx)
    }
    out
  }*/
}









/**
 * This is the conversion of the original SPL to the non-pointfree version
 */
trait SPL_Base extends Base  {

  //=============================================================================
  // SPL Operators
  //=============================================================================
  implicit def SPLtoRep (i: SPL): Rep[SPL] = unit(i)
  //implicit def rReptoRep (i: rSPL): Rep[rSPL] = unit(i)


  implicit def mkSPLRepOps(lhs: Rep[SPL]): SPLOps = new SPLOps(lhs)
  class SPLOps (x: Rep[SPL]) {
    def tensor(y: Rep[SPL]) = infix_tensor(x,y)

    def compose(y: Rep[SPL]) = infix_compose(x,y)
    //def compose[T:Manifest](y: Rep[T]) = infix_compose(x,y)
    def directsum(y: Rep[SPL]) = infix_directsum(x,y)
    //def apply(y: Rep[Matrix]) = infix_compose(x,y)


  }
  def infix_tensor(x: Rep[SPL], y: Rep[SPL]): Rep[SPL]
  //def infix_compose[T:Manifest](x: Rep[SPL], y: Rep[T]): Rep[T]
  def infix_compose(x: Rep[SPL], y: Rep[SPL]): Rep[SPL]
  def infix_directsum(x: Rep[SPL], y: Rep[SPL]): Rep[SPL]

  //def transpose(x: Rep[SPL]): Rep[SPL]
}



trait SPL_Exp extends SPL_Base with BaseExp with DSLBaseTypes {

  case class Tensor(x: Exp[SPL], y: Exp[SPL])(implicit ctx: SourceContext) extends Def[SPL]
  case class Compose(x: Exp[SPL], y: Exp[SPL])(implicit ctx: SourceContext) extends Def[SPL]
  //case class Compose[T] (x: Exp[SPL], y: Exp[T])(implicit ctx: SourceContext) extends Def[T]
  case class DirectSum(x: Exp[SPL], y: Exp[SPL])(implicit ctx: SourceContext) extends Def[SPL]
  //case class Transpose(x: Exp[rSPL])(implicit ctx: SourceContext) extends Def[SPL]

  def infix_tensor(x: Exp[SPL], y: Exp[SPL]) =  Tensor(x,y)
  def infix_compose(x: Exp[SPL], y: Exp[SPL]) = Compose(x,y)
  def infix_directsum(x: Exp[SPL], y: Exp[SPL]) = DirectSum(x, y)




  //def transpose(x: Rep[SPL]) = Transpose(x)


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Tensor(x,y) => infix_tensor(f(x),f(y))
    case Compose(x,y) => infix_compose(f(x),f(y))
    case DirectSum(x,y) => infix_directsum(f(x),f(y))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
}

class SPL_DSL extends SPL_Exp








