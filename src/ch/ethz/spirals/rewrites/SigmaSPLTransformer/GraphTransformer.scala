/**
 *  SpiralS - ETH Zurich
 *  Copyright (C) 2013  Leo Büttiker   (leob@ethz.ch)
 *                      Georg Ofenbeck (ofenbeck@inf.ethz.ch)
 *                      Alen Stojanov  (astojanov@inf.ethz.ch)
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

package ch.ethz.spirals.rewrites.SigmaSPLTransformer

import scalaz.{Tree, TreeLoc}
import scala.collection.mutable.Set

trait GraphTransformer extends PatternLibrary {

  case class GraphTransformer (ast:TreeLoc[Code]) {
//  case class GraphTransformer (full_graph:Tree[Code]) {
    import TreeUtil._

    var localAST: TreeLoc[Code] = ast
    var foundPatterns: List[PatternChecker] = List()

    // implicite the root of a pattern has to be a Instruction or a BigCompose
    def findInitialHandles (pattern: Tree[Pattern], ast: TreeLoc[Code]): Stream[TreeLoc[Code]] = pattern.rootLabel match {
      case InstructionPattern(pOp) =>
        findAll(ast, (t:TreeLoc[Code]) => t.getLabel match {
          case Instruction(op) => op == pOp
          case _ => false
        })
      case _ => findAll(ast, (t:TreeLoc[Code]) => t.getLabel match {
        case _:BigCompose => pattern.rootLabel == MultiCompose
        case _ => false
      })
    }

    def expandInitialHandles (initialHandles: Stream[TreeLoc[Code]], pattern: Tree[Pattern]) =
      initialHandles map { x => PatternChecker(x, pattern) }


    def transform (transformations:(Tree[Pattern],Tree[Pattern])*):GraphTransformer = {
      transformations foreach {
        case (from,to) => transform(from,to)
      }
      this
    }

    def find (pattern:Tree[Pattern]): Stream[PatternChecker] = {
      val initialHandles = findInitialHandles(pattern,localAST)
      val patternCheckers = expandInitialHandles(initialHandles, pattern)
      patternCheckers filter (_.found)
    }

    def find(patterns:List[Rule]):Map[Rule,Stream[PatternChecker]] = {
      patterns.map{ x => (x,find(x.from))}.toMap
    }

    def replace(places:Map[Rule,Stream[PatternChecker]])  = places foreach { case (rule,pcs) =>
      pcs foreach { pc =>
        val newCodeTree = PatternUtil.pattern2codetree(rule.to, pc.terminalMap)
        localAST = if(pc.codetree.isRoot) newCodeTree.loc else TreeUtil.replace(newCodeTree, pc.codetree).root
      }
    }

    def transform (from:Tree[Pattern], to:Tree[Pattern]): GraphTransformer = {
      var inserted = false
      do {
        inserted = false
        val newFoundPatterns = find(from).reverse
        foundPatterns :::= newFoundPatterns.toList
        newFoundPatterns foreach {
          case pc@PatternChecker(foundSubGraph, _) => {
            val newCodeTree = PatternUtil.pattern2codetree(to, pc.terminalMap)
            inserted = true
            localAST = if(foundSubGraph.isRoot)
              newCodeTree.loc
            else
            {
              val id = foundSubGraph.tree.rootLabel.sym.get.id
              GraphUtil.subtrees += id -> newCodeTree
              TreeUtil.replace(newCodeTree, foundSubGraph).root
            }
          }
        }
      } while( inserted )

      this
    }
  }

  case class Rule (name: String, from: Tree[Pattern], to: Tree[Pattern])

  case class PatternChecker (codetree: TreeLoc[Code], pattern: Tree[Pattern]) {

    private val guards: Set[Guard] = Set()
    var terminalMap:Map[String,Tree[Code]] = Map()
    def updateNames(t:Tree[Code], p:Pattern) = p.name match {
      case Some(name) => terminalMap += name -> t
      case None =>
    }
    def guarded(og:Option[Guard],n:Code):Boolean = (og, n.originalRef) match {
      case (Some(guard:FunGuard),Some(ref)) => guard.check(ref)
      case (Some(guard:EqualGuard),_) => !(guards add guard)
      case _ => false
    }

    import Tree.Node
    //Implementation notes/idea: http://stackoverflow.com/a/16563123/55070
    def check(pattern:Tree[Pattern], graph:Tree[Code]):Boolean = {
      updateNames(graph,pattern.rootLabel)
      if(guarded(pattern.rootLabel.guard,graph.rootLabel)) return false;
      (pattern, graph) match {
        case (Node(_,_), Node(CSX(loc),Stream.Empty))  => check(pattern,GraphUtil.subtrees(loc))
        case (Node(Terminal(_),Stream.Empty),        Node(_,_)                            )             => true
        case (Node(IConstant(pi),Stream.Empty),      Node(IntegerConstant(i,_),Stream.Empty)) if(pi == i) => true
        case (Node(BConstant(pb),Stream.Empty),      Node(BooleanConstant(b),Stream.Empty)) if(pb == b) => true
        case (Node(MultiCompose,pc),           		 Node(_:BigCompose,gc)) 					=>
          checkList(pc,gc)
        case (whatever,           		 Node(CodeBlock,gc)) 					=>
          assert(gc.length == 1, "A code block has always only one child")
          check(whatever,gc(0))
        case (Node(InstructionPattern(poperand),pc), Node(Instruction(operand),gc)        ) if(poperand == operand) =>
          (pc zip gc).forall{p => (check _).tupled(p)}
        case _ => false
      }
    }

    def checkList(pattern:Stream[Tree[Pattern]], graph:Stream[Tree[Code]]):Boolean = (pattern, graph) match {
      case ((p@Node(Star(name),_))#::Stream.Empty,Stream.Empty) => {
        //set, resp. reset (in case of backtracking) the terminal map
        terminalMap += name -> Tree.node(BigCompose(), Stream())
        true //only a star left
      }
      case ((p@Node(Star(name),_))#::ps,c#::cs) =>
        if(checkList(p#::ps,cs)) {
          //did eat a "c" and match. add it to the name terminal map
          terminalMap.get(name) match {
            case Some(Node(_:BigCompose, ss)) => terminalMap += name -> Tree.node(BigCompose(), c#::ss)
            case None => terminalMap += name -> Tree.node(BigCompose(), Stream(c))
          }
          true
        } else if(checkList(ps,c#::cs)) {
          //set, resp. reset (in case of backtracking) the terminal map
          terminalMap += name -> Tree.node(BigCompose(), Stream())
          true
        } else {
          false
        }
      case (p#::ps,c#::cs) if(check(p,c))   => checkList(ps,cs)
      case (Stream.Empty,Stream.Empty)      => true //pattern fully matched with no leftovers
      case _ => false
    }

    val found = check(pattern,codetree.tree) && !(guards map {
      case EqualGuard(l,r) => (terminalMap(l),terminalMap(r)) match {
        case (Node(IntegerConstant(lc,_),Stream.Empty),Node(IntegerConstant(rc,_),Stream.Empty)) => lc == rc
        case _ => false
      }
      case _ => false} exists(!_))
  }
}
