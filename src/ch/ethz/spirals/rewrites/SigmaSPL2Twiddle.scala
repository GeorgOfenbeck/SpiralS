package ch.ethz.spirals.rewrites
import scala.virtualization.lms.internal.{GraphVizExport, NestedBlockTraversal}
import collection.immutable.HashMap

import ch.ethz.spirals.util.{NestedBlockTraversalOrdered, Utilities}
import scala.collection.mutable.ListBuffer
import ch.ethz.spirals.dsls.cir.CIRCodegen

/**
 * Georg Ofenbeck
 First created:
 * Date: 30/07/13
 * Time: 15:26 
 */
abstract class SigmaSPL2Twiddle extends NestedBlockTraversalOrdered   { self =>
  import ch.ethz.spirals.dsls._
  val IR: ch.ethz.spirals.dsls.SigmaSPL_DSL
  import IR._


  class CIR_DSL_IR extends CIR_DSL { self =>
    val codegen = new CIRCodegen {
      val IR: self.type = self
    }
  }
  val CIR = new CIR_DSL_IR ()

  var mapSigmaSPLBaseSymToIntF = HashMap.empty[Exp[Any], (Unit => Int)] //here we construct the chain that will yield the value
  var mapSigmaSPLBaseSymToInt = HashMap.empty[Exp[Any], Int] //here we update the actual value
  import scala.collection.mutable.Map
  val idx2fmap = Map[Exp[Any], Int => Int]  ()
  var im2fmap = HashMap.empty[Exp[Any], Int => Int]

  val TwiddleList_re : ListBuffer[Double] = ListBuffer()
  val TwiddleList_im : ListBuffer[Double] = ListBuffer()



  def getSymValue(exp: Rep[Int]): (Unit => Int) = exp match
  {
    case s@Sym(_) => mapSigmaSPLBaseSymToIntF(s)
    case Const(x: Int) => (s:Unit) => x
  }

  override def traverseStm(stm: Stm): Unit = stm match {

    case TP(s, Sigma(start, Const(end), sigmai, body)) => {
      val endi = end.asInstanceOf[Int]
      mapSigmaSPLBaseSymToIntF += sigmai -> {(x: Unit) => mapSigmaSPLBaseSymToInt(sigmai)}
      for (i <- 0 until endi)
      {
        mapSigmaSPLBaseSymToInt += sigmai -> i
        traverseBlock(body)
      }
    }

    case TP(s, SPL_S_RaderDiag(Const(n:Int),Const(k:Int),Const(root:Int),im: Rep[IndexMapping], in)) => {
      import org.apache.commons.math3.complex.{ComplexFormat, ComplexField, Complex}
      import org.apache.commons.math3.transform.{TransformType, DftNormalization, FastFourierTransformer}
      import ch.ethz.spirals.util._

      val inv = getDSLVector(in)
      val size = inv.size


      val tarrayplus2 = ch.ethz.spirals.util.Registry.getRaderTwidd(n, k, root)
      val IM = emitIM(im)
      for (i <- 0 until size)
      {
        val tidx = IM(i)
        val t_complex = new Complex(tarrayplus2((tidx)*2),-tarrayplus2((tidx)*2+1))
        val d_complex = if (tidx >= 2) t_complex.divide(n-1) else t_complex
        val re = (d_complex.getReal())
        val im = (d_complex.getImaginary())
        TwiddleList_re.append(re)
        TwiddleList_im.append(im)

      }
    }
    case TP(s, SPL_S_T3L(Const(n:Int),Const(d:Int),Const(k:Int),im: Rep[IndexMapping], in)) => {
      val inv = getDSLVector(in)
      val size = inv.size
      val diag  = ch.ethz.spirals.util.Registry.getDiagTensor2(n,d)
      val t = ch.ethz.spirals.util.Registry.getTwidd(2*n: Int)
      val IM = emitIM(im)
      for (i <- 0 until size)
      {
        val tidx = IM(i)
        val diagv = diag(tidx)
        val re = t.re(diagv.toInt*k)
        val im = t.im(diagv.toInt*k)
        TwiddleList_re.append(re)
        TwiddleList_im.append(im)
      }
    }

    case TP(s, SPL_S_T(Const(n:Int),Const(d:Int),Const(k:Int),im: Rep[IndexMapping], in)) => {
      val inv = getDSLVector(in)
      val size = inv.size
      val diag  = ch.ethz.spirals.util.Registry.getDiagTensor(n,d)
//      implicit object DoubleX extends CIR.NumericRep.DoubleX
      //val t = CIR.E[Double](n)
      val t = ch.ethz.spirals.util.Registry.getTwidd(n: Int)
      //val root_list_re = diag map ( ele => t.re(ele.toInt*k))
      //val root_list_im = diag map ( ele => t.im(ele.toInt*k))

      val IM = emitIM(im)
      //println("Hae? ",size)
      for (i <- 0 until size)
      {
        val tidx = IM(i)
        val diagv = diag(tidx)
        val re = t.re(diagv.toInt*k)
        val im = t.im(diagv.toInt*k)
        //val re = root_list_re(tidx)
        //val im = root_list_im(tidx)
        TwiddleList_re.append(re)
        TwiddleList_im.append(im)
      }
    }

    case TP(s_im, IM_V(Const(m: Int),Const(k: Int), range, domain)) => im2fmap += (s_im -> ((i: Int) => {


      val floor = i/m
      val t1 = m * floor
      val bracket = i % m
      val t2 = bracket * k
      val t3 =  t1 + t2
      val mod = t3 % (m*k)
      //val gmod = numeric_times(i,)
      mod
    }))

    case TP(s_im, IM_Z(b,s, range, domain)) => im2fmap += (s_im -> ((i: Int) => {
      (getSymValue(b)() + i *  getSymValue(s)()) % domain
    }))

    case TP(s_im, IM_H(b, s, _, _)) => im2fmap += (s_im -> ((i: Int) => {
      getSymValue(b)() + i *  getSymValue(s)()
    }))
    case TP(s_im, IM_L(k, m, _, _)) => im2fmap += (s_im -> ((i: Int) => {
      assert(false)
      i
    }))
    case TP(s_im, IM_Compose (a1, b1, _, _)) => im2fmap += (s_im -> ((i: Int) => {
      val xf = im2fmap.get(a1).get
      val yf = im2fmap.get(b1).get
      xf(yf(i))
    }))

    case TP(s_im, IM_WT(Const(phi: Int),Const(g: Int), range, domain)) => im2fmap += (s_im -> ((i: Int) => {
      val gpow = scala.math.pow(g,i) //infix_pow(g,i)
      val gphi = gpow * phi //numeric_times(gpow,phi)
      val gmod = gphi.toInt % domain //infix_mod(gphi,size)
      gmod

    }))

    case TP(s_im, IM_W(Const(g: Int), range: Int, domain: Int)) => im2fmap += (s_im -> ((i: Int) => {
      val gpow = scala.math.pow(g,i-1) //infix_pow(g,numeric_minus(i,1))
      val gmod = gpow.toInt % domain // infix_mod(gpow,size)
      gmod
    }))

    case TP(s_im, IM_Twiddle (Const(d: Int), Const(n: Int), range: Int, domain: Int)) => im2fmap += (s_im -> ((i: Int) => i))

      /*
    case TP(s, IM_Compose(a1,b1)) =>
    {
      (a1,b1) match {
        case (Const(x: IndexMapping), s1@Sym(n)) =>
          val f : Int => Int = (i : Int) =>
          {
            val yf = idx2fmap(s1)
            val xf = emitidxfunction(x)
            xf(yf(i))
          }
          idx2fmap += (s -> f)
        case (s1@Sym(n),Const(x: IndexMapping)) =>
          val f : Int => Int = (i : Int) =>
          {
            val yf = idx2fmap(s1)
            val xf = emitidxfunction(x)
            yf(xf(i))
          }
          idx2fmap += (s -> f)
        case (s1@Sym(n1), s2@Sym(n)) =>
          val f : Int => Int = (i : Int) =>
          {
            val yf = idx2fmap(s2)
            val xf = idx2fmap(s1)
            xf(yf(i))
          }
          idx2fmap += (s -> f)
        case (Const(x: IndexMapping), Const(y: IndexMapping)) =>
          val f : Int => Int = (i : Int) =>
          {
            val yf = emitidxfunction(y)
            val xf = emitidxfunction(x)
            xf(yf(i))
          }
          idx2fmap += (s -> f)
      }
    }
                 */

    case TP(s, IntTimes  (lhs: Rep[Int], rhs: Rep[Int])) =>
    {
      val v = (something: Unit) =>  getSymValue(lhs)() * getSymValue(rhs)()
      mapSigmaSPLBaseSymToIntF += s -> v
    }
    case TP(s, IntPlus  (lhs: Rep[Int], rhs: Rep[Int])) =>
    {
      val v = (something: Unit) =>  getSymValue(lhs)() + getSymValue(rhs)()
      mapSigmaSPLBaseSymToIntF += s -> v
    }
    case TP(s, IntMinus  (lhs: Rep[Int], rhs: Rep[Int])) =>
    {
      val v = (something: Unit) =>  getSymValue(lhs)() - getSymValue(rhs)()
      mapSigmaSPLBaseSymToIntF += s -> v
    }
    case TP(s, IntDivide  (lhs: Rep[Int], rhs: Rep[Int])) =>
    {
      val v = (something: Unit) =>  getSymValue(lhs)() / getSymValue(rhs)()
      mapSigmaSPLBaseSymToIntF += s -> v
    }

    case TP(s, NumericPlus  (lhs: Rep[Int], rhs: Rep[Int])) =>
      {
        val v = (something: Unit) =>  getSymValue(lhs)() + getSymValue(rhs)()
        mapSigmaSPLBaseSymToIntF += s -> v
      }
    case TP(s, NumericMinus  (lhs: Rep[Int], rhs: Rep[Int])) =>
    {
      val v = (something: Unit) =>  getSymValue(lhs)() - getSymValue(rhs)()
      mapSigmaSPLBaseSymToIntF += s -> v
    }
    case TP(s, NumericTimes  (lhs: Rep[Int], rhs: Rep[Int])) =>
    {
      val v = (something: Unit) =>  getSymValue(lhs)() * getSymValue(rhs)()
      mapSigmaSPLBaseSymToIntF += s -> v
    }
    case TP(s, NumericDivide  (lhs: Rep[Int], rhs: Rep[Int])) =>
    {
      val v = (something: Unit) =>  getSymValue(lhs)() / getSymValue(rhs)()
      mapSigmaSPLBaseSymToIntF += s -> v
    }
    case TP(s, Pow  (lhs: Rep[Int], rhs: Rep[Int])) =>
    {
      val v = (something: Unit) =>  scala.math.pow(getSymValue(lhs)() , getSymValue(rhs)()).toInt
      mapSigmaSPLBaseSymToIntF += s -> v
    }


    case _ => {
      //println("seems we dont have a translation for ", stm)

      super.traverseStm(stm)
    }
  }

  def emitIM(idx: Rep[IndexMapping]) : ((Int) => Int) = im2fmap.get(idx).get
/*
  def emitIndexFunction(idx: Rep[IndexMapping]) : Int => Int =
  {

    idx match    {
      case s@Sym(n) =>     idx2fmap.get(s).get
      case Const(x: IndexMapping) => emitidxfunction(x)
      case _ => assert(false, "Unknown index mapping funciton " + idx); idx2fmap.get(0).get    }
  }
  def emitidxfunction(idxfunction: IndexMapping) : Int => Int =
  {
    idxfunction match {
      case (twiddle : stwiddle) => (i: Int) => i
      case (hfunction: sh) => emith(hfunction)
      case _ => assert(false, "Unknown idx function " + idxfunction); (in: Int) => in
    }
  }




  def emith(hfunction: sh) : Int => Int =
  {
    hfunction match
    {
      case sh(b,s) => {
        val cb = getSymValue(b)
        val cs = getSymValue(s)

        val f = (i: Int) =>  {
          val out = cb() + i * cs()
          out
        }
        f
      }
    }
  }*/


}
