/**
 *  SpiralS - ETH Zurich
 *  Copyright (C) 2013  Georg Ofenbeck (ofenbeck@inf.ethz.ch)
 *
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
package ch.ethz.spirals.scoring

import perfplot._
import java.io.StringWriter
import ch.ethz.spirals.db.Queries
import ch.ethz.spirals.dsls.cir.CIRCodegen

object Scoring {



  def getScore(bd: ch.ethz.spirals.rewrites.BreakDown, validate: Boolean = true) = {
    // import ch.ethz.spirals.rewrites.SigmaSPL2CIR
    import ch.ethz.spirals.rewrites.SigmaSPLTransformer.SigmaSPLTransformer
    import ch.ethz.spirals._
    import ch.ethz.spirals.dsls._
    import ch.ethz.spirals.rewrites._
    import ch.ethz.spirals.util._
    import java.io.PrintWriter
    import scala.virtualization.lms.common._
    import scala.virtualization.lms.internal._
    import ch.ethz.spirals.util._


    val Debug = false
    val plot_graphs = true
    val print_latex = false

    val dftsize = bd.nt.size

    val profiling = Profiling()


    val lintree = Queries.Breakdown2LinTree(bd)

    val latex_file = if (print_latex)
      new java.io.PrintWriter(new java.io.FileOutputStream("dft"+dftsize+"_" + lintree + "_SPL.tex"))
    else
      null


    def plat (s: String) = {
      if (print_latex)
        latex_file.println(s)
    }


    plat("\\documentclass[landscape]{article}\\usepackage[utf8]{inputenc}\n\\usepackage[T1]{fontenc}\n\\usepackage{amsmath}\n\\usepackage{amssymb}\n\\usepackage[usenames]{color}\n\\usepackage{graphicx}\n\\usepackage[scaled=0.8]{luximono}\n\\usepackage{url}\n\\usepackage{listings}\n\\usepackage{paralist}\n\\usepackage[compact]{titlesec}\n\\usepackage{booktabs}\n\n\\usepackage[x11names, rgb]{xcolor}\n\\usepackage[utf8]{inputenc}\n\\usepackage{tikz}\n\\usetikzlibrary{snakes,arrows,shapes}\n\\usepackage{amsmath}\n\\usepackage{hyperref}\n\n% ----- Spiral shortcuts\n\\newcommand{\\DFT}[0]{\\operatorname{\\bf DFT}}\n\n\\newcommand{\\dft}[0]{F}\n\\newcommand{\\tensor}[0]{\\otimes}\n\\newcommand{\\dirsum}[0]{\\oplus}\n\\newcommand{\\bigdirsum}[0]{\\bigoplus}\n\\newcommand{\\diag}[0]{\\operatorname{diag}}\n\\newcommand{\\C}[0]{{\\mathbb{C}}}\n\\newcommand{\\dps}[0]{\\displaystyle}\n\\newcommand{\\twiddle}{T}\n\\newcommand{\\one}[0]{I}\n\\newcommand{\\stride}{L}\n\\newcommand{\\crtperm}{V}\n\\newcommand{\\raderperm}{W}\n\n\\newcommand{\\sspl}{$\\Sigma$-SPL\\xspace}\n\n\\newcommand{\\ts}[0]{\\textstyle}\n\\newcommand{\\nont}[1]{\\text{$\\langle\\text{#1}\\rangle$}}\n\\newcommand{\\oder}[0]{$\\ |\\ $}\n\\newcommand{\\function}[4]{#1:#2\\rightarrow #3;\\ i\\mapsto #4}\n\\newcommand{\\interval}{\\mathbb{I}}\n\\newcommand{\\fInterval}[1]{\\interval_{#1}}\n\\newcommand{\\II}[1]{\\interval_{#1}}\n\\newcommand{\\Afunction}[4]{#1 &:\\; #2\\rightarrow #3;\\ i\\mapsto #4}\n\\newcommand{\\fH}[4]{h_{#3,\\;#4}} \n\\newcommand{\\Gath}[1]{\\ensuremath{G(#1)}}\n\\newcommand{\\Scat}[1]{\\ensuremath{S(#1)}}\n\\newcommand{\\Perm}[1]{\\ensuremath{\\operatorname{perm}(#1)}}\n\\newcommand{\\Diag}[1]{\\ensuremath{\\diag\\big({#1}\\big)}}\n\n\n\n\\graphicspath{{figures/}}\n\n\\setlength{\\floatsep}{6pt plus 2pt minus 2pt}\n\\setlength{\\textfloatsep}{6pt plus 2pt minus 2pt}\n\n\\begin{document}% Define styles for bags and leafs\n\\tikzstyle{bag} = [text width=10em, text centered]\n\\tikzstyle{end} = [circle, minimum width=3pt,fill, inner sep=0pt]")





    profiling.start("Generation_time_bd2spl")
    //-----------------------------SPL
    if (Debug) println("Translating Breakdown to SPL")
    val dsl = new SPL_DSL
    val spltransformation = new BreakDown2SPL with BreakDown2Latex with ExportGraph with SPL2SigmaSPL with SPL2Mat with SPL2Latex{
      val IR: SPL_Exp = dsl
    }
    val spl = spltransformation.bd2spl(bd)
    profiling.stop


    if (plot_graphs)
    {
      profiling.start("Generation_time_emitSPLGraph")
      spltransformation.emitDepGraph(spl,"dpdft"+dftsize+"_SPL.graph")
      profiling.stop
    }

    if (print_latex)
    {
      profiling.start("Generation_time_emitSPLLatex")
      //plat(spltransformation.bd2latex(bd))
      val lat = spltransformation.SPL2Latex(spl)
      plat("$$")
      plat(lat.last)
      plat("$$")
      profiling.stop
    }



    //-----------------------------------sigma SPL
    profiling.start("Generation_time_SPL2SigmaSPL")
    if (Debug) println("Translating SPL to SigmaSPL")
    val initSigmaSPLIR = new SigmaSPL_DSL
    val sigmaf = spltransformation.SPL2SigmaSPL(spl, initSigmaSPLIR)
    val init_input = new initSigmaSPLIR.Vector(dftsize)
    val init_output = sigmaf(init_input)
    profiling.stop



    //-----------------------------------sigma SPL Transformation

    val (transformed_input, transformed_output, transformedSigmaSPLIR) = if ( true ) {
      if (Debug) println("Rewriting SigmaSPL")
		object Func2BlockInput extends ForwardTransformer{
        val IR: initSigmaSPLIR.type = initSigmaSPLIR
        import IR._
        def funcToBlock(in: List[Any], out: Any) = {
          val (inputs, t) = (in, out).asInstanceOf[Tuple2[List[DSLType], DSLType]]
          (inputs, reifyBlock[DSLType](t.rep))
        }
      }
    profiling.start("Generation_time_SigmaSPL2Block_rewrite")
		val (arguments, init_block) = Func2BlockInput.funcToBlock(List(init_input), init_output)
    profiling.stop

    if (plot_graphs) {
        profiling.start("Generation_time_emitSigmaSPLGraph_before_rewrite")
        val p = new ExportGraph {          val IR : initSigmaSPLIR.type = initSigmaSPLIR       }
        p.emitDepGraph(init_block.res,"dpdft"+dftsize+"_SigmaSPL_before.graph")
        profiling.stop
    }

    profiling.start("Generation_time_SigmaSPL_rewrite")
      val newSigmaSPLIR = new SigmaSPL_DSL ()
      val transformer = new SigmaSPLTransformer {
        val IR: initSigmaSPLIR.type = initSigmaSPLIR
        val newIR: newSigmaSPLIR.type = newSigmaSPLIR
        //val inputs = List(init_input)
        //val output = init_output
      }
      val (a, b) = transformer.transform(List(init_input), init_output, init_block)

//      transformer.transform(init_block)
      profiling.stop
      (a(0), b, newSigmaSPLIR)
    } else {
      (init_input, init_output, initSigmaSPLIR)
    }




    //-----------------------------------sigma SPL to Block
    println("Conversion of SigmaSPL to block")
    profiling.start("Generation_time_SigmaSPL2Block")
    object Func2Block extends ForwardTransformer{
      val IR: transformedSigmaSPLIR.type = transformedSigmaSPLIR
      import IR._
      def funcToBlock(in: List[Any], out: Any) = {
        val (inputs, t) = (in, out).asInstanceOf[Tuple2[List[DSLType], DSLType]]
        (inputs, reifyBlock[DSLType](t.rep))
      }
    }
    val (arguments, block) = Func2Block.funcToBlock(List(transformed_input), transformed_output)
    profiling.stop

    val block2 = block

    /*if (plot_graphs) {
      profiling.start("Generation_time_emitSigmaSPLGraph_after_rewrite")
      val p = new ExportGraph {        val IR : transformedSigmaSPLIR.type = transformedSigmaSPLIR      }
      p.emitDepGraph(block2.res,"dpdft"+dftsize+"_SigmaSPL_after.graph")
      profiling.stop
    }*/


    
    //------------------------------ Conversion to C
    profiling.start("Generation_time_TwiddlePrecompute")
    //unrolling
    val unroll = new SigmaSPLBase_Unrolling {
      val IR: SigmaSPL_DSL = transformedSigmaSPLIR
    }
    val unrolled_block = unroll.transform(block.asInstanceOf[unroll.Block[unroll.IR.DSLType]])

    if (plot_graphs) {
      profiling.start("Generation_time_emitSigmaSPLGraph_after_rewrite")
      val p = new ExportGraph {        val IR : unroll.IR.type = unroll.IR      }
      p.emitDepGraph(unrolled_block.res,"dpdft"+dftsize+"_SigmaSPL_after.graph")
      profiling.stop
    }


    val calc_twiddles = new SigmaSPL2Twiddle {
      val IR: SigmaSPL_DSL =  transformedSigmaSPLIR
    }
    calc_twiddles.traverseBlock(unrolled_block.asInstanceOf[calc_twiddles.Block[calc_twiddles.IR.DSLType]])
    profiling.stop
    profiling.start("Generation_time_CIR")
    class CIR_DSL_IR extends CIR_DSL { self =>
      val codegen = new CIRCodegen {
        val IR: self.type = self
      }
    }
    val CIR = new CIR_DSL_IR ()

    object trans extends SigmaSPL2CIR(CIR.ComplexNumberFactories.InterleavedDoubles){ self =>

      val IR :unroll.IR.type = unroll.IR
      val C: CIR.type = CIR

      TwiddleList_re = calc_twiddles.TwiddleList_re.toList
      TwiddleList_im = calc_twiddles.TwiddleList_im.toList


      val biggestT = dftsize

      // val CDT = CIR.InterleavedAComplexVectorFactory
    }

    implicit val (f, mList) = trans.transform(arguments, unrolled_block.asInstanceOf[trans.Block[trans.IR.DSLType]])
    implicit val mUnit = manifest[Unit]

    var codeBlock = CIR.CIR_Optimizer.funcToBlock(f)
    codeBlock = CIR.CIR_Optimizer.transform(codeBlock)
    profiling.stop



    profiling.start("Generation_time_CIR2File")
    val writer = new StringWriter()
    val pwriter = new PrintWriter(writer)
    CIR.codegen.emitTransformedBlock(codeBlock,"staged", pwriter)
    profiling.stop

    
      
    

    val (c_dft,filename) = CIR.compileTransformedBlock(codeBlock)(mList, manifest[Unit])
    import ch.ethz.spirals.validation._
    object tester extends DFTCodeTester {
      val CIR_DSL_Object = CIR
    }


    val perfplotTiming = false


    def validateit () =
    {
      bd.nt match {
        case DFT(n,k) =>
          if (k == 1 && validate == true)
          {
            println("Comparing code to external code (output)")
            tester.verify(c_dft, dftsize)
            CIR.unloadProgram(filename)
          }
          else
          {

            println("skipping validation for DFT("+n+","+k+") cause this is not supported by JTransform")
          }
        case _ => assert(false) // this should not happen
      }
    }



    if (perfplotTiming)
    {
      profiling.start("Generation_time_Perfplot")
      val counters = HWCounters.JakeTown
      val code_string : String = writer.toString
      val fft_kernel = SpiralS_perfplot("someid",code_string,true,true,bd.nt.size,false).setFlopCounter(counters.flops_double)
      val flags = perfplot.Config.flag_optimization + perfplot.Config.flag_hw + perfplot.Config.flag_novec
      perfplot.Config.delete_temp_files = false
      val kernel_res = perfplot.CommandService.fromScratch("DPSearch", fft_kernel, flags)
      val tsc = kernel_res.getTSC()
      val flops = kernel_res.getFlops(fft_kernel)
      profiling.stop

      profiling.start("Generation_time_Validation")
      validateit()
      profiling.stop

      plat("\\end{document}")
      if (print_latex) latex_file.close()
      (tsc,flops,profiling)
    }
    else
    {
      profiling.start("Generation_time_Validation")
      validateit()
      profiling.stop
      val tsc = tester.getExecutionTime()
      val flops: List[Long] = List(1,1,1)
      plat("\\end{document}")
      if (print_latex) latex_file.close()
      (tsc,flops,profiling)

    }
  }

  /**
   *
   * @param id
   * @param code
   * @param double_precision
   * @param warm
   * @param size
   * @param inplace
   * @return
   */

  def SpiralS_perfplot(id: String, code: String, double_precision: Boolean, warm: Boolean, size: Long, inplace: Boolean ): perfplot.CodeGeneration =
  {

    val fft = new CodeGeneration
    fft.id = if (double_precision)
      "SpiralS_" + id
    else
      "SpiralS_" + id
    fft.size = size.toInt
    fft.total_size = if (inplace)  (2 * size).toInt else  (4 * size).toInt

    fft.includes = "#include <iostream>\n#include <fstream>\n#include <cstdlib>\n#include <ctime>\n#include <cmath>\n"
    fft.initcode = ""
    //fft.initcode = "spiral_t dummy;"

    if (double_precision)
      fft.datatype = "double"
    else
      fft.datatype = "float"

    fft.create_buffer_call = if (warm)
    {
      fft.datatype +" * x = (" + fft.datatype + " *) _mm_malloc("+2*size+"*sizeof(" + fft.datatype + "),ALIGNMENT);" +
        (if (!inplace) fft.datatype +" * y = (" + fft.datatype + " *) _mm_malloc("+2*size+"*sizeof(" + fft.datatype + "),ALIGNMENT);" else "")
    }
    else
    {
      fft.datatype + " ** x_array = (" + fft.datatype + " **) CreateBuffers("+2*size+"* sizeof(" + fft.datatype + "),numberofshifts);" +
        (if (!inplace) fft.datatype + " ** y_array = (" + fft.datatype + " **) CreateBuffers("+2*size+"* sizeof(" + fft.datatype + "),numberofshifts);" else "")
    }

    fft.create_buffer_function = fft.create_array_of_buffers()
    fft.destroy_buffer_function = fft.destroy_array_of_buffers()
    fft.init_function = fft.ini11() //initializes with 1.1 everything - suboptimal

    fft.init_call = if (warm)
    {
      "_ini1(x,"+size+" ,1);" +
        (if (!inplace) "_ini1(y,"+size+" ,1);" else "")
    }
    else
    {
      "for(int i = 0; i < numberofshifts; i++){" +
        "_ini1(x_array[i],"+size+" ,1);" +
        (if (!inplace) "_ini1(y_array[i],"+size+" ,1);" else "") +
        "}"
    }

    //fft.kernel_header = "typedef struct {\n\tdouble* input;\n\tdouble* output;\n} spiral_t;\nvoid staged(spiral_t* );"
    fft.kernel_header = "void staged(double *x, double * y );"
    fft.kernel_code = code
    fft.inline = false //compile it seperate to avoid deadcode eliminiation
    fft.kernel_call = if (warm){
      if (inplace)
      {
        assert(false) //not implemented yet
          "staged(x);"
      }
      else
      {
          "staged(x,y);"
      }
    }
    else
    {
      if (inplace)
      {
        assert(false) //not implemented yet
        "dummy.input = x;\ndummy.output = y;" +
          "staged(&dummy);"
      }
      else
      {
        //"dummy.input = x_array[i%numberofshifts];\ndummy.output = y_array[i%numberofshifts];" +
          "staged(x_array[i%numberofshifts],y_array[i%numberofshifts]);"
      }
    }

    fft.destroy_buffer_call = if (warm)
    {
      "_mm_free(x);"+
        (if (!inplace) "_mm_free(y);" else "")
    }
    else
      "DestroyBuffers( (void **) x_array, numberofshifts);" +
        (if (!inplace) "DestroyBuffers( (void **) y_array, numberofshifts);" else "")

    fft.determineSize_call = fft.tuneNrRunsbySize()
    fft.nrRuns = fft.tuneNrRunsbyRunTime()
    fft
  }


}
