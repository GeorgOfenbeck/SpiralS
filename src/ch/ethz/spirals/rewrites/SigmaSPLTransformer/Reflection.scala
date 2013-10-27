/**
 *  SpiralS - ETH Zurich
 *  Copyright (C) 2013  Leo Büttiker   (leob@ethz.ch)
 *                      Alen Stojanov  (astojanov@inf.ethz.ch)
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
package ch.ethz.spirals.rewrites.SigmaSPLTransformer

import scala.reflect.SourceContext
import scala.collection._
import ch.ethz.spirals.datatypes.DSLBaseTypes
import ch.ethz.spirals.db.DB

trait Reflection {

  val IR: DSLBaseTypes

  class ReflectionMethodDoesNotExist(msg: String) extends Exception (msg)

  private lazy val IRMethods = IR.getClass().getMethods.toList

  val IntClass = classOf[Int]
  val IntegerClass = classOf[Integer]
  type Method = java.lang.reflect.Method

  val methodMap: mutable.HashMap[AnyRef, Method] = mutable.HashMap.empty[AnyRef, Method]

  // CODE from: https://gist.github.com/xeno-by/4985929
  // Based on: http://stackoverflow.com/questions/11020746/get-companion-object-instance-with-new-scala-reflection-api
  def getCompanionObject(t: Product):AnyRef = DB.synchronized {
    import scala.reflect.runtime.{currentMirror => cm}
    //TODO this is a bit black magic
    val outerMethod = t.getClass.getMethods.filter(_.getName.endsWith("""$outer""")).headOption
    val moduleMirror = outerMethod.map{ _.invoke(t) }.map{ cm.reflect(_) }
    val instanceSymbol = cm.classSymbol(t.getClass)
    val companionSymbol = instanceSymbol.companionSymbol.asModule
    val companionMirror =
      moduleMirror.
        map{ _.reflectModule(companionSymbol) }.
        getOrElse{ cm.reflectModule(companionSymbol) }
    val instance = companionMirror.instance
    instance.asInstanceOf[AnyRef]
  }

  def invokeApply(companionObject:AnyRef, params:List[Object]) = {
    //  TODO searching the apply method is a bit shaky for now
    //  println(companionObject +" with "+ params)
    val applyMethod = (companionObject.getClass.getMethods.toList filter{m => (m.getName == "apply") && m.getModifiers == 1}).head
    applyMethod.invoke(companionObject, params:_*)
  }

  private def filterMethods (methodName: String, params: List[Object], chkReturn: Boolean) = IRMethods filter { method =>
    // match the name of the method
    val nameMatch = method.getName.toLowerCase == methodName
    // match the number of arguments
    val pTypes = method.getParameterTypes ()
    val s = pTypes.size
    val pMatch = ( s == params.size ) || (s == params.size + 1 && pTypes(s-1).getName == "scala.reflect.SourceContext" )
    // match the return type
    val returnType = !chkReturn || method.getReturnType.getSimpleName == "Exp"
    // check all constraints now
    if(nameMatch && pMatch && returnType) {
      // match the the argument types
      (method.getParameterTypes().toList zip params.toList.map{p => p.getClass}).map{
        case (IntClass, IntegerClass) 	=> true
        case (x,y)						          => x.isAssignableFrom(y)
      }.fold(true)(_ && _)
    } else false
  }

  // In case of override methods, calculate the distance between the
  // IR class and the super-class where the method is implemented
  private def superDistance (a: Class[_], b: Class[_]): Int = {
    if (a eq b) 0 else 1 + superDistance(a, b.getSuperclass())
  }

  // Find all methods that fit the parameters and output the last overriden method
  private def findMethod (mName: String, params: List[Object], checkReturnType: Boolean) = {
    filterMethods(mName, params, checkReturnType).map(method =>
      (method, superDistance(method.getDeclaringClass, IR.getClass()))
    ).sortBy (_._2)
  }

  def invoke_DSL_infix (companionObject:AnyRef, params: List[Object], checkReturnType: Boolean = true) = {
	
    val methodFound = if ( methodMap.contains(companionObject) ) {
      methodMap.get(companionObject)
    } else {
      val name = companionObject.getClass.getSimpleName.replace("$", "")

      val possibleMethodNames = List (
        "infix_" + name.toLowerCase,
        name.toLowerCase.replace("numeric", "numeric_"),
        name.toLowerCase,
        name.toLowerCase.replace("int", "int_")
      )

      val methodCandidates = possibleMethodNames.flatMap ( methodName => {
        findMethod(methodName, params, checkReturnType).map(x => x._1)
      })
      if ( methodCandidates.isEmpty ) {
        throw new ReflectionMethodDoesNotExist (possibleMethodNames.toString)
      }
      methodMap.update(companionObject, methodCandidates.lift(0).get)
      methodCandidates.lift(0)
    }

    //println(possibleMethodNames)
    methodFound match {
      case Some(method: java.lang.reflect.Method) => {
        if ( method.getParameterTypes.size == params.size ) {
          method.invoke(IR, params:_*)
        } else {
          method.invoke(IR, (params :+ implicitly[SourceContext]):_*)
        }
      }
      case None => {
        IR.getClass().getMethods.toList.map (x => println(x))
        throw new ReflectionMethodDoesNotExist (companionObject.getClass.getSimpleName.replace("$", ""))
      }
    }
  }
}
