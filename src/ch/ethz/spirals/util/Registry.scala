package ch.ethz.spirals.util

import ch.ethz.spirals.dsls.CIR_DSL
import ch.ethz.spirals.dsls.cir.CIRCodegen

//ALL the global in SpiralS are bad enough, with the registry we have 
//at least a common access method
//Code base from http://stackoverflow.com/a/1094214/55070
object Registry {
  import scala.reflect.Manifest

  private var map= Map.empty[Any,(Manifest[_], Any)] 

  def register[T](name: Any, item: T)(implicit m: Manifest[T]) {
    map = map.updated(name, m -> item)
  }
  
  //another way to call the method: Registry.register("key" -> value)
  def register[T](nameItemTuple:(Any,T))(implicit m: Manifest[T]) {
    map = map.updated(nameItemTuple._1, m -> nameItemTuple._2)
  }

  def get[T](key:Any)(implicit m : Manifest[T]): Option[T] = {
    map get key flatMap {
      case (om, s) => if (om <:< m) Some(s.asInstanceOf[T]) else None
    }     
  }


  class CIR_DSL_IR extends CIR_DSL { self =>
    val codegen = new CIRCodegen {
      val IR: self.type = self
    }
  }
  val CIR = new CIR_DSL_IR ()


  case class RaderTwiddleCache(val n: Int, val k: Int, val root: Int)
  private val RaderTwiddleMap = scala.collection.mutable.Map.empty[RaderTwiddleCache,Array[Double]]
  def getRaderTwidd(n: Int, k: Int, root: Int): Array[Double] = {
    RaderTwiddleMap.get(RaderTwiddleCache(n,k, root)).getOrElse(
    {
      println("Creating new Rader.E[Double] " + n + " " + k)
      val t = ch.ethz.spirals.util.Registry.getTwidd(n: Int)
      val x_re = (0 until n-1).toList map (index =>  t.re(MathUtilities.mathmod((k * BigInt(root).pow(index) ), n) ))
      val x_im = (0 until n-1).toList map (index =>  t.im(MathUtilities.mathmod((k * BigInt(root).pow(index) ), n) ))
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

      println(tarray.size)
      //doing this cause the assumption is that we just pass through the first 2 elements that are actually handled through rader mid
      val tarrayplus2 = new Array[Double](tarray.size+2)
      for (i <- 1 until tarrayplus2.size/2)
      {
        tarrayplus2(i*2) = tarray((i-1)*2)
        tarrayplus2(i*2+1) = tarray((i-1)*2+1)
      }
      for (i <- 0 until 2)
      {
        tarrayplus2(i*2) = 1
        tarrayplus2(i*2+1) = 0 //will use the negative later on
      }



      RaderTwiddleMap += RaderTwiddleCache(n,k,root) -> tarrayplus2
      tarrayplus2
    }
    )
  }


  case class TwiddleCache(val n: Int)
  private val TwiddleMap = scala.collection.mutable.Map.empty[TwiddleCache,CIR.E[Double]]
  def getTwidd(n: Int): CIR.E[Double] = {
    TwiddleMap.get(TwiddleCache(n)).getOrElse(
    {
      println("Creating new E[Double] "+n)

      val t: CIR.E[Double] = CIR.E[Double](n)(CIR.ImplicitOps.DoubleNoRep)
      TwiddleMap += TwiddleCache(n) -> t
      t
    }
    )
  }

  case class DiagCache(val n: Int, val d: Int)
  private val DiagTensorMap = scala.collection.mutable.Map.empty[DiagCache,List[Double]]
  def getDiagTensor(n : Int, d: Int): List[Double] = {
    DiagTensorMap.get(DiagCache(n,d)).getOrElse(
    {
      println("Creating DiagTensor "+n + " " + d)
      val t: List[Double] = MathUtilities.diagTensor(MathUtilities.dLin(n/d,1,0), MathUtilities.dLin(d,1,0))
      DiagTensorMap += DiagCache(n,d) -> t
      t
    }
    )
  }

  case class DiagCache2(val n: Int, val d: Int)
  private val DiagTensorMap2 = scala.collection.mutable.Map.empty[DiagCache2,List[Double]]
  def getDiagTensor2(n : Int, d: Int): List[Double] = {
    DiagTensorMap2.get(DiagCache2(n,d)).getOrElse(
    {
      println("Creating DiagTensor2 "+n + " " + d)
      val t: List[Double] = MathUtilities.diagTensor(MathUtilities.dLin(n/d,1,0), MathUtilities.dLin(d,2,1)).grouped(2).toList.transpose.flatten //this is the L(n/2,k) part
      DiagTensorMap2 += DiagCache2(n,d) -> t
      t
    }
    )
  }





}
