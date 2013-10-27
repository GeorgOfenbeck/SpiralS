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

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._


/**
 * Georg Ofenbeck
 First created:
 * Date: 31/05/13
 * Time: 12:25 
 */
trait SPL2SigmaSPL extends SPL2Size with GraphTraversal {
  import ch.ethz.spirals.dsls._

  val IR: SPL_Exp
  import IR._


  val tag_startsize = 32//TODO - REMOVE ME!!!!!

  //-----------------------------------------Matrix Representation Part --------------------------------
  def SPL2SigmaSPL (start: Exp[Any], sigmaspl: SigmaSPL_DSL) = {
    import scala.collection.mutable.HashMap
    
    def emittag_startPos(sym: Sym[Any], rhs: Def[Any], fmap : Array[List[Int]], lt : HashMap[Int,Int]): (List[Int])  = rhs match
    {
      case Compose(a1,b1) =>
        (a1,b1) match {
          case (Sym(a),Sym(b)) =>  fmap(lt(a)) ++ fmap(lt(b))
          case (Sym(a),Const(b: SPL))=>  fmap(lt(a))
          case (Const(a: SPL),Sym(b))=>  fmap(lt(b))
          case (Const(a: SPL),Const(b: SPL))=> List():List[Int]
        }
      case DirectSum(a1,b1) =>
        (a1,b1) match {
          case (Sym(a),Sym(b)) =>  fmap(lt(a)) ++ fmap(lt(b))
          case (Sym(a),Const(b: SPL))=>  fmap(lt(a))
          case (Const(a: SPL),Sym(b))=>  fmap(lt(b))
          case (Const(a: SPL),Const(b: SPL))=> List():List[Int]
        }
      case Tensor(a1,b1) =>
      {
        val tensor_size = emitSize(sym)
        if (tensor_size <= tag_startsize)
          List(sym.id)
        else{
          (a1,b1) match {
            case (Const(I(n)),Sym(b)) => fmap(lt(b))
            case (Const(I(n)),Const(b: SPL)) => List():List[Int]
            case (Sym(a),Const(I(n))) => fmap(lt(a))
            case (Const(a: SPL),Const(I(n))) => List():List[Int]
          }
        }
      }
    }


    val deflist = buildScheduleForResult(start)
    //val size = emitSize(start) //returns size in terms of SPL

    //we actually do 2 passes - one inital pass to see where to put the tags for tag_starting

    var farray3 = new Array[List[Int]](deflist.size)
    val index_array2 = new HashMap[Int,Int]
    var i : Int = 0
    for (TP(sym, rhs) <- deflist) {
      val index: Int = sym match {
        case Sym(n) => n
        case _ => -1
      }
      index_array2 += (index -> i)
      farray3(i) = emittag_startPos(sym,rhs,farray3,index_array2)
      i = i + 1
    }


    println("Positions of tag_start tags")
    farray3.last map println
    println("!Positions of tag_start tags")


    var farray = new Array[sigmaspl.Vector => sigmaspl.Vector](deflist.size)
    val index_array = new HashMap[Int,Int]





    i = 0
    for (TP(sym, rhs) <- deflist) {
      val index: Int = sym match {
        case Sym(n) => n
        case _ => -1
      }
      index_array += (index -> i)
      farray(i) = emitSigmaSPL(sym,rhs,farray,index_array,farray3)
      i = i + 1
    }


    def directsum(fa: sigmaspl.Vector => sigmaspl.Vector, fb: sigmaspl.Vector => sigmaspl.Vector, asize: Int, bsize: Int) =
    {

      val f : sigmaspl.Vector => sigmaspl.Vector = (in: sigmaspl.Vector) => {
        val tsize = asize + bsize
        //println("Direct Sum size: ", asize, bsize, tsize)
        import sigmaspl._
        val repasize: sigmaspl.Rep[Int] = asize
        val repbsize: sigmaspl.Rep[Int] = bsize

        val resa =
        {
          val gattered = G(H(0,1, asize,tsize),in)
          val processed = fa(gattered)
          val scattered = S(H(0,1,asize,tsize),processed)
          scattered
        }
        val resb =
        {
          val gattered = G(H(repasize,1, bsize,tsize),in)
          val processed = fb(gattered)
          val scattered = S(H(repasize,1, bsize,tsize),processed)
          scattered
        }
        resa ++ resb
      }
      f
    }




    def emitSigmaSPL(sym: Sym[Any], rhs: Def[Any], fmap : Array[sigmaspl.Vector => sigmaspl.Vector], lt : HashMap[Int,Int], tag_start_pos: Array[List[Int]]): (sigmaspl.Vector => sigmaspl.Vector)  = rhs match
    {
      //The big difference in the two DSL is, that in SigmaSPL SPL expressions are operators,
      //while in SPL we just treat them as objects (matrices)

      //--------------------------------Compose -----------------------------
      case Compose(a1,b1) =>
      {
        (a1,b1) match {
          case (Sym(a),Sym(b)) => (in: sigmaspl.Vector) => fmap(lt(a))(fmap(lt(b))(in))
          case (Sym(a),Const(b: SPL))=> (in: sigmaspl.Vector) => fmap(lt(a))(emitSigmaSPLOperator(b,in))
          case (Const(a: SPL),Sym(b))=> (in: sigmaspl.Vector) => emitSigmaSPLOperator(a,fmap(lt(b))(in))
          case (Const(a: SPL),Const(b: SPL))=> (in: sigmaspl.Vector) => emitSigmaSPLOperator(a,emitSigmaSPLOperator(b,in))
        }
      }

      case DirectSum(a1,b1) =>
      {
        (a1,b1) match {
          case (Sym(a),Sym(b)) => {
            val asize = emitSize(a1)
            val bsize = emitSize(b1)
            val fa: (sigmaspl.Vector => sigmaspl.Vector) = (ina: sigmaspl.Vector) => fmap(lt(a))(ina)
            val fb: (sigmaspl.Vector => sigmaspl.Vector) = (inb: sigmaspl.Vector) => fmap(lt(b))(inb)
            directsum(fa, fb, asize, bsize)
          }
          case (Sym(a),Const(b: SPL))=> {
            val asize = emitSize(a1)
            val bsize = b.size
            val fa: (sigmaspl.Vector => sigmaspl.Vector) = (ina: sigmaspl.Vector) => fmap(lt(a))(ina)
            val fb: (sigmaspl.Vector => sigmaspl.Vector) = (inb: sigmaspl.Vector) => emitSigmaSPLOperator(b,inb)
            directsum(fa, fb, asize, bsize)
          }
          case (Const(a: SPL),Sym(b))=> {
            val asize = a.size
            val bsize = emitSize(b1)
            val fa: (sigmaspl.Vector => sigmaspl.Vector) = (ina: sigmaspl.Vector) => emitSigmaSPLOperator(a,ina)
            val fb: (sigmaspl.Vector => sigmaspl.Vector) = (inb: sigmaspl.Vector) => fmap(lt(b))(inb)
            directsum(fa, fb, asize, bsize)
          }
          case (Const(a: SPL),Const(b: SPL))=>
          {
            val asize = a.size
            val bsize = b.size
            val fa: (sigmaspl.Vector => sigmaspl.Vector) = (ina: sigmaspl.Vector) => emitSigmaSPLOperator(a,ina)
            val fb: (sigmaspl.Vector => sigmaspl.Vector) = (inb: sigmaspl.Vector) => emitSigmaSPLOperator(b,inb)
            directsum(fa, fb, asize, bsize)
          }
        }
      }


      case Tensor(a1,b1) =>
      {
        val tensor_size = emitSize(sym)
        (a1,b1) match {
          case (Const(I(n)),Sym(b)) => {(in: sigmaspl.Vector) =>
          {
            import sigmaspl._
            val frag_size: sigmaspl.Rep[Int] = tensor_size/n
            val loop = sigma(n,
              i => {
                val gattered = if (tag_start_pos.last.contains(sym.id)) G(H(frag_size*i,1, tensor_size/n, tensor_size), tag_end(in, Unroll())) else G(H(frag_size*i,1, tensor_size/n, tensor_size), in)
                val processed = fmap(lt(b))(gattered)
                val scattered = S(H(frag_size*i,1, tensor_size/n, tensor_size),processed)
                scattered
              })
            if (tag_start_pos.last.contains(sym.id)) sigmaspl.tag_start(loop,Unroll()) else loop
          }}
          case (Const(I(n)),Const(b: SPL)) => {(in: sigmaspl.Vector) =>
          {
            import sigmaspl._
            val frag_size: sigmaspl.Rep[Int] = tensor_size/n
            val loop = sigma(n,
              i => {
                val gattered = if (tag_start_pos.last.contains(sym.id)) G(H(frag_size*i,1, tensor_size/n, tensor_size),tag_end(in, Unroll())) else G(H(frag_size*i,1, tensor_size/n, tensor_size), in)
                val processed = emitSigmaSPLOperator(b,(gattered))
                val scattered = S(H(frag_size*i,1, tensor_size/n, tensor_size),processed)
                scattered
              })
            if (tag_start_pos.last.contains(sym.id)) sigmaspl.tag_start(loop,Unroll()) else loop
          }}
          case (Sym(a),Const(I(n))) => {(in: sigmaspl.Vector) =>
          {
            import sigmaspl._
            val loop = sigma(n,
              i => {
                val gattered = if (tag_start_pos.last.contains(sym.id)) G(H(i,n,tensor_size/n, tensor_size), tag_end(in, Unroll())) else G(H(i,n,tensor_size/n, tensor_size), in)
                val processed = fmap(lt(a))(gattered)
                val scattered = S(H(i,n, tensor_size/n, tensor_size),processed)
                scattered
              })
            if (tag_start_pos.last.contains(sym.id)) sigmaspl.tag_start(loop, Unroll()) else loop
          }}
          case (Const(a: SPL),Const(I(n))) => {(in: sigmaspl.Vector) =>
          {
            import sigmaspl._
            val loop = sigma(n,
              i => {
                val gattered = if (tag_start_pos.last.contains(sym.id)) G(H(i,n, tensor_size/n, tensor_size),tag_end(in, Unroll())) else G(H(i,n, tensor_size/n, tensor_size), in)
                val processed = emitSigmaSPLOperator(a,(gattered))
                val scattered = S(H(i,n, tensor_size/n, tensor_size),processed)
                scattered
              })
            if (tag_start_pos.last.contains(sym.id)) sigmaspl.tag_start(loop, Unroll()) else loop
          }}
        }
      }
      case _ => {
        assert(false, "Unmatched translation in SPL2SigmaSPL " + sym)
        (in: sigmaspl.Vector) => in
      }
    }

    def emitSigmaSPLOperator(spl : SPL, in: sigmaspl.Vector): sigmaspl.Vector =
    {

      if (spl.size != in.size) assert(false, "Operator " + spl.toString + " size does not match input size: " +in.size)
      //println("Operator " + spl.toString + " size  -> input size: " +in.size)
      val out: sigmaspl.Vector = spl match {
        case T(n,d,k) => sigmaspl.infix_SPL_T(n,d,k,in)
        case T3L(n,d,k) => sigmaspl.infix_SPL_T3L(n,d,k,in)
        case Rader_Diag(n,k,root) => sigmaspl.infix_SPL_RaderDiag(n,k,root,in)
        case I(n) => sigmaspl.infix_SPL_I(n,in)

        case L(n,k) => sigmaspl.infix_SPL_L(n,k,in)
        case W(n,phi,g) => sigmaspl.infix_SPL_W(n,phi,g,in)
        case Wt(n,phi,g) => sigmaspl.infix_SPL_Wt(n,phi,g,in)
        case V(r,s,alpha,beta) => sigmaspl.infix_SPL_V(r,s,alpha,beta,in)
        case Vt(r,s,alpha,beta) => sigmaspl.infix_SPL_Vt(r,s,alpha,beta,in)

        case Rader_Mid_Matrix(n) => sigmaspl.infix_SPL_RaderMid(n,in)
        case F_2() => sigmaspl.infix_SPL_F2(in)
        case D2(k) => sigmaspl.infix_SPL_D2(k,in)

        case _ => {
          assert(false,"no matching SigmaSPL operator for " + spl)
          sigmaspl.infix_SPL_L(1,1,in)
        }
      }

      out
    }





    farray(i-1)
  }








}



