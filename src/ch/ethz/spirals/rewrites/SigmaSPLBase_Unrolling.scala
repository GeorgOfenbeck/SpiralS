package ch.ethz.spirals.rewrites

import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._
import ch.ethz.spirals.util.SubstitutionTransformer

/**
 * Georg Ofenbeck
 First created:
 * Date: 18/09/13
 * Time: 15:33 
 */
abstract class SigmaSPLBase_Unrolling extends NestedBlockTraversal {
  self =>

  import ch.ethz.spirals.dsls._
  val IR: ch.ethz.spirals.dsls.SigmaSPL_DSL
  import IR._

  var unrolledExp = Set.empty[Exp[Any]]
  def ifUnroll(exp: Exp[Any]) = unrolledExp.contains(exp)

  def transform(b1: Block[DSLType]) = {
    val block = preUnroll(b1)
    block
  }

  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(s, TagEnd(x, Unroll()))        => super.traverseStm(stm)
    case TP(s, TagStart(body, Unroll()))    => unrolledExp += body; super.traverseStm(stm)
    case TP(s, d) if ifUnroll(s) => syms(d).foreach(x => unrolledExp += x); super.traverseStm(stm)
    case _ => super.traverseStm(stm)
  }


  override def traverseStmsInBlock[A](stms: List[Stm]): Unit =  stms.reverse foreach traverseStm


  def preUnroll (block: Block[DSLType]) = {

    /*var (foundNest, bx) = (false, block)
    var removeroll = 0
    val removenests = new ForwardTransformer {
      val IR: self.IR.type = self.IR
      def getSubst(body: Block[Vector], sym: Sym[Any], replacement: Exp[Any]): Exp[Vector] = {
        val t = new SubstitutionTransformer { val IR: self.IR.type = self.IR }
        t.register(sym, replacement)
        getBlockResult(t.transformBlock(body))
      }
      override def transformStm(stm: Stm): Exp[Any] = stm match {
        case TP(s, Roll(body))  if ifUnroll(s) && (removeroll > 0)  => {
          foundNest = true
          removeroll = removeroll - 1
          body
        }
        case TP(s, Unroll(body))  if ifUnroll(s)  =>
        {
          foundNest = true
          removeroll = removeroll + 1
          body
        }
        case _ => super.transformStm(stm)
      }
    }
    do {
      // restart the unrolling annotation
      unrolledExp = Set.empty[Exp[Any]]
      traverseBlock(bx)
      // initialize the transformer
      foundNest = false
      bx = removenests.transformBlock(bx)
    } while (foundNest)

*/


    //var (foundSigma, b) = (false, bx)
    var (foundSigma, b) = (false, block)
    do {
      // restart the unrolling annotation
      unrolledExp = Set.empty[Exp[Any]]
      traverseBlock(b)
      // initialize the transformer
      foundSigma = false



      val transformer = new ForwardTransformer {
        val IR: self.IR.type = self.IR
        def getSubst(body: Block[Vector], sym: Sym[Any], replacement: Exp[Any]): Exp[Vector] = {
          val t = new SubstitutionTransformer { val IR: self.IR.type = self.IR }
          t.register(sym, replacement)
          getBlockResult(t.transformBlock(body))
        }
        override def transformStm(stm: Stm): Exp[Any] = stm match {

          //case TP(s, Roll(Unroll(body))) => infix_SPL_I(1,body)

          case TP(s, Sigma(start, end, i, body)) if ifUnroll(s) => (end - start) match {
            case Const (range: Int) => {
              foundSigma = true
              var result = infix_directsum(getSubst(body, i, start), getSubst(body, i, start + 1))
              for (it <- 2 until range) result = infix_directsum(result, getSubst(body, i, start + it))
              result
            }
            case _ => assert(false, "Loop can not be unrolled");
          }
          //case TP(s, Sigma(start, end, i, body))  => println("Loop " + s + "not in map")


          case TP(s, SigmaSum(start, end, i, body)) if ifUnroll(s) => (end - start) match {
            case Const (range: Int) => {
              foundSigma = true
              var result = infix_sum(getSubst(body, i, start), getSubst(body, i, start + 1))
              for (it <- 2 until range) result = infix_sum(result, getSubst(body, i, start + it))
              result
            }
            case _ => assert(false, "Loop can not be unrolled");
          }
          case _ => super.transformStm(stm)
        }
      }
      b = transformer.transformBlock(b)
    } while (foundSigma)
    b
  }

}
