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

package ch.ethz.spirals.validation

import org.bridj.{DynamicFunction, Pointer}
import ch.ethz.spirals.dsls.{CIR_DSL}
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D
import scala.util.Random
import ch.ethz.spirals.util.MathUtilities._
import ch.ethz.spirals._
import ch.ethz.spirals.conf._

abstract class DFTCodeTester {

  val CIR_DSL_Object: CIR_DSL
  import CIR_DSL_Object._

  private var compileTime: Long = Long.MaxValue
  private var executionTime: Long = Long.MaxValue
  private var fft_program: DynamicFunction[Nothing] = null
  private var second_program: DynamicFunction[Nothing] = null

  private val measurementCycles = 10
  private val verificationCycles = 10
  private val random = new Random(Config().spiralsconfig.seed)


  private def execute2 (input: Array[Double]): Array[Double] = {
    val xBridJ = Pointer.allocateArray[java.lang.Double](java.lang.Double.TYPE, input.size)
    val yBridJ = Pointer.allocateArray[java.lang.Double](java.lang.Double.TYPE, input.size)

    for (i <- 0 until input.size) xBridJ.set(i, input(i))

    val yOutput = new Array[Double](input.size)
    for (i <- 0 until (input.size)) {
      yOutput(i) = yBridJ.as(java.lang.Double.TYPE).get(i)
    }

    var eTime = System.nanoTime
    second_program(xBridJ,yBridJ)
    eTime = System.nanoTime - eTime
    executionTime = if (eTime < executionTime) eTime else executionTime
    //val output = new Array[Double](input.size)
    for (i <- 0 until input.size) {
      yOutput(i) = yBridJ.as(java.lang.Double.TYPE).get(i)
    }
    Pointer.release(xBridJ)
    Pointer.release(yBridJ)

    yOutput
  }

  private def execute (input: Array[Double]): Array[Double] = {
    val xBridJ = Pointer.allocateArray[java.lang.Double](java.lang.Double.TYPE, input.size)
    val yBridJ = Pointer.allocateArray[java.lang.Double](java.lang.Double.TYPE, input.size)

    for (i <- 0 until input.size) xBridJ.set(i, input(i))

    val yOutput = new Array[Double](input.size)
    for (i <- 0 until (input.size)) {
      yOutput(i) = yBridJ.as(java.lang.Double.TYPE).get(i)
    }

    var eTime = System.nanoTime
    fft_program(xBridJ,yBridJ)
    eTime = System.nanoTime - eTime
    executionTime = if (eTime < executionTime) eTime else executionTime
    //val output = new Array[Double](input.size)
    for (i <- 0 until input.size) {
      yOutput(i) = yBridJ.as(java.lang.Double.TYPE).get(i)
    }
    Pointer.release(xBridJ)
    Pointer.release(yBridJ)

    yOutput
  }
  /*
    private def executeNoIO(c_input_ptr: Pointer[spiral_t]) : Unit = {
      var eTime = System.nanoTime
      fft_program(c_input_ptr)
      eTime = System.nanoTime - eTime
      executionTime = if (eTime < executionTime) eTime else executionTime
    }*/

  private def executeWithJTransforms(input: Array[Double], size: Int): Array[Double] = {
    var data = input.clone()
    for ( i <- 0 until size) {
      data(i * 2 + 1) = -input(i * 2 + 1)
    }
    if ( Config().validationconfig.useFFTW ) {
      data = ch.ethz.spirals.validation.fftw3j_1D.fftw3j_1D(data, size)
    } else {
      val transformer = new DoubleFFT_1D (size)
      transformer.complexForward(data)
    }
    for ( i <- 0 until size) {
      data(i * 2 + 1) = -data(i * 2 + 1)
    }
    data
  }

  private def verify (size: Int): Unit = {
    for ( cycle <- 0 until verificationCycles ) {
      var input = new Array[Double](size * 2)
      for ( i <- 0 until size ) {
        input(i) = random.nextDouble()
      }
      val cOutput = execute(input)
      val vOutput = executeWithJTransforms(input, size)
      val allowed_error:Double = 1E-6
      var error:Double = 0
      for ( i <- 0 until size * 2 ) {
        error = error + Math.abs(cOutput(i) - vOutput(i))
      }
      if (error > allowed_error)
      {
        if (size > 16)
          assert (error < allowed_error, "Verification of FFT(" + size.toString + ") failed with error of: " + error.toString)
        else //show the full matrix
        {
          import org.apache.commons.math3.linear.BlockFieldMatrix
          import org.apache.commons.math3.complex.{ComplexField, Complex}

          val reconstruct_matrix = new BlockFieldMatrix[Complex](ComplexField.getInstance(),size,size)
          for (i <- 0 until size)
          {
            var input = new Array[Double](size * 2)
            input(i*2) = 1
            val cOutput = execute(input)
            //cOutput map println
            for (j <- 0 until (cOutput.size/2) )
              reconstruct_matrix.setEntry(i,j,new Complex(cOutput(j*2),cOutput(j*2+1)))
            //val vOutput = executeWithJTransforms(input, size)
          }
          println("DFT from code")
          ch.ethz.spirals.util.MathUtilities.printm(reconstruct_matrix)
          //println("DFT by definition")
          //val x= ch.ethz.spirals.dsls.DFT(size,1)


          assert (error < allowed_error, "Verification of FFT(" + size.toString + ") failed with error of: " + error.toString)

        }
      }


    }
  }


  private def compare (size: Int): Unit = {
    for ( cycle <- 0 until verificationCycles ) {
      var input = new Array[Double](size * 2)
      for ( i <- 0 until size ) {
        input(i) = random.nextDouble()
      }

      /*def swap () = {
        var t: DynamicFunction[Nothing] = null
        t = fft_program
        fft_program = second_program
        second_program = t
      }
      swap()*/
      val cOutput = execute(input)
      val vOutput = execute2(input)
      val allowed_error:Double = 1E-6
      var error:Double = 0
      for ( i <- 0 until size * 2 ) {
        error = error + Math.abs(cOutput(i) - vOutput(i))
      }
      if (error > allowed_error)
      {
        if (size > 16)
          assert (error < allowed_error, "Verification of SPL(" + size.toString + ") failed with error of: " + error.toString)
        else //show the full matrix
        {
          import org.apache.commons.math3.linear.BlockFieldMatrix
          import org.apache.commons.math3.complex.{ComplexField, Complex}
          {
            val reconstruct_matrix = new BlockFieldMatrix[Complex](ComplexField.getInstance(),size,size)
            for (i <- 0 until size)
            {
              var input = new Array[Double](size * 2)
              input(i*2) = 1

              val cOutput = execute(input)
              //cOutput map println
              for (j <- 0 until (cOutput.size/2) )
                reconstruct_matrix.setEntry(i,j,new Complex(cOutput(j*2),cOutput(j*2+1)))
              //val vOutput = executeWithJTransforms(input, size)
            }
            println("DFT from code version1")
            ch.ethz.spirals.util.MathUtilities.printm(reconstruct_matrix)
          }
          {
            val reconstruct_matrix = new BlockFieldMatrix[Complex](ComplexField.getInstance(),size,size)
            for (i <- 0 until size)
            {
              var input = new Array[Double](size * 2)
              input(i*2) = 1

              val cOutput = execute2(input)
              //cOutput map println
              for (j <- 0 until (cOutput.size/2) )
                reconstruct_matrix.setEntry(i,j,new Complex(cOutput(j*2),cOutput(j*2+1)))
              //val vOutput = executeWithJTransforms(input, size)
            }
            println("DFT from code version2")
            ch.ethz.spirals.util.MathUtilities.printm(reconstruct_matrix)
          }
          //println("DFT by definition")
          //val x= ch.ethz.spirals.dsls.DFT(size,1)


          assert (error < allowed_error, "Verification of FFT(" + size.toString + ") failed with error of: " + error.toString)

        }
      }


    }
  }




  private def measure (size: Int): Long = {

    /*
    val c_input = new spiral_t ()
    val c_input_ptr = Pointer.pointerTo(c_input)
    c_input.input(Pointer.allocateArray[java.lang.Double](java.lang.Double.TYPE, size * 2))
    c_input.output(Pointer.allocateArray[java.lang.Double](java.lang.Double.TYPE, size * 2))
    for (i <- 0 until measurementCycles) {
      executeNoIO(c_input_ptr)
    }
    Pointer.release(c_input.input())
    Pointer.release(c_input.output())
    Pointer.release(c_input_ptr)
    getExecutionTime() */

    1 //FIXME!
  }

  def verifyBlock (block: (List[CIR_DSL_Object.Sym[Any]], CIR_DSL_Object.Block[Unit]), size: Int)(implicit mList: List[Manifest[Any]]) = {
    init()
    compileTime = System.nanoTime
    fft_program = CIR_DSL_Object.compileTransformedBlock[Unit](block)
    compileTime = System.nanoTime - compileTime
    verify(size)
  }

  /*def verify (code: CIR_DSL_Object.Exp[CIR_DSL_Object.SpiralStruct] => CIR_DSL_Object.Exp[Unit], size: Int) {
    init()
    fft_program = CIR_DSL_Object.compile(code)//CIR_DSL_Object.compileOptimized(code);
    verify(size)
  }*/


  def verify ( fft: DynamicFunction[Nothing], fft2: DynamicFunction[Nothing] ,size: Int) {
    init()
    fft_program = fft//CIR_DSL_Object.compileOptimized(code);
    second_program = fft2
    compare(size)
  }


  def verify ( fft: DynamicFunction[Nothing],size: Int) {
    init()
    fft_program = fft//CIR_DSL_Object.compileOptimized(code);
    verify(size)
  }

  /*def measureTime(code: CIR_DSL_Object.Exp[CIR_DSL_Object.SpiralStruct] => CIR_DSL_Object.Exp[Unit], size: Int): Long = {
    init()
    compileTime = System.nanoTime
    fft_program = CIR_DSL_Object.compile(code)
    compileTime = System.nanoTime - compileTime
    measure(size)
  }*/

  def measureTimeBlock(block: (List[CIR_DSL_Object.Sym[Any]], CIR_DSL_Object.Block[Unit]), size: Int)(implicit mList: List[Manifest[Any]]): Long = {
    init()
    compileTime = System.nanoTime
    fft_program = CIR_DSL_Object.compileTransformedBlock[Unit](block)
    compileTime = System.nanoTime - compileTime
    measure(size)
  }

  def init () {
    compileTime = Long.MaxValue
    executionTime = Long.MaxValue
//    if ( fft_program != null ) {
//      CIR_DSL_Object.unloadProgram(fft_program)
//      fft_program = null
//    }
  }

  def getCompileTime () : Long = {
    return compileTime
  }

  def getExecutionTime () : Long = {
    return executionTime
  }

}
