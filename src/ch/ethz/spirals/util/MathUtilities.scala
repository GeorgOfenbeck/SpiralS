/**
 *  SpiralS - ETH Zurich
 *  Copyright (C) 2013  Georg Ofenbeck (ofenbeck@inf.ethz.ch)
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


package ch.ethz.spirals.util

import scala.math._
import org.apache.commons.math3.linear._
import org.apache.commons.math3.complex.{ComplexFormat, ComplexField, Complex}

object MathUtilities {

  def printm(m: org.apache.commons.math3.linear.BlockFieldMatrix[org.apache.commons.math3.complex.Complex]) = {
    for (i <- 0 until m.getRowDimension){
      for (j <- 0 until m.getColumnDimension)
      {
        val x = m.getEntry(i,j)
        if (abs(x.getImaginary()) < 1E-14)
          m.setEntry(i,j,new org.apache.commons.math3.complex.Complex(x.getReal(),0))
        val y = m.getEntry(i,j)
        if (abs(y.getReal()) < 1E-14)
          m.setEntry(i,j,new org.apache.commons.math3.complex.Complex(0,y.getImaginary))
        val t = m.getEntry(i,j)
        if (t.getReal() == 0 && t.getImaginary() == 0)
          print("(    ,    )")
        else
          print("(% 2.1f;% 2.1f)".format(t.getReal(),t.getImaginary))
      }
      println
    }
  }


  def dLin(N : Int,a : Double,b : Double) : List[Double] = {
    val t_array = new Array[Double](N)
    for (i <- 0 until N)
      t_array(i) = a*i + b
    t_array.toList
  }

  def diagTensor(a : List[Double], b: List[Double]): List[Double] = {
    val t_array = new Array[Double](a.size * b.size)
    for (i <- 0 until a.size)
      for (j <- 0 until b.size)
        t_array(i*b.size+j) = a(i) * b(j)
    t_array.toList
  }


  //this just takes care that the mod on negative numbers is defined the math way
  //not taking care of fractions yet
  def mathmod(number: BigInt, mod: Int): Int =
  {
    if (number < 0)
      mod + (number % mod).toInt
    else
      (number % mod).toInt
  }


  def mathmod (number : Int, mod : Int) =
  {
    if (number < 0)
      mod + number % mod
    else
      number % mod
  }

  def CompareMatrices(a : BlockFieldMatrix[Complex], b: BlockFieldMatrix[Complex]) =
  {
    if (a.getColumnDimension != b.getColumnDimension || a.getRowDimension != b.getColumnDimension)
      -1
    else
    {
      var max = 0.0
      for (i <- 0 until a.getRowDimension)
        for (j <- 0 until a.getColumnDimension)
        {
          val ae = a.getEntry(i,j)
          val be = b.getEntry(i,j)

          val diff = ae.subtract(be)
          val diff_abs = diff.abs()
          if (diff_abs > max          )
            max = diff_abs
        }
      max
    }

  }

/*## -----------------------------------------------------------------------------
  #F CRT(<r>, <s>) - Chinese remainder theorem permutation function of size r*s
   This is the tranposed version
  ## */

  def CRT_transpose (r: Int, s: Int, alpha: Int, beta: Int) =
  {
    val N = r*s
    val s1 : BigInt = s
    val r1 : BigInt = r
    //val aa = mathmod(1/s/alpha , r)
    val aa = s1.modInverse(r)
    //val bb = mathmod(1/r/beta, s)
    val bb = r1.modInverse(s)
    (0 until N).toList map (i => ( ( (s*alpha* (i/s).floor ) + (r*beta* (i%s) ) )   % N).toInt )
  }

  /*## -----------------------------------------------------------------------------
 #F CRT(<r>, <s>) - Chinese remainder theorem permutation function of size r*s

 ## */

  def CRT (r: Int, s: Int, alpha: Int, beta: Int) =
  {
    val N = r*s
    val s1 : BigInt = s
    val r1 : BigInt = r
    //val aa = mathmod(1/s/alpha , r)
    val aa = s1.modInverse(r)
    //val bb = mathmod(1/r/beta, s)
    val bb = r1.modInverse(s)
    //(0 until N).toList map (i => ( ( (s*alpha* (i/s).floor ) + (r*beta* (i%s) ) )   % N) )
    (0 until N).toList map (i => ( s* ((i*aa)%r) + ((i*bb)%s)).toInt)
  }


/*#############################################################################
  ##
  #F  IsPrimeInt( <n> ) . . . . . . . . . . . . . . . . . . .  test for a prime
  ##
  ##  'IsPrimeInt' returns 'false'  if it can  prove that <n>  is composite and
  ##  'true' otherwise.  By  convention 'IsPrimeInt(0) = IsPrimeInt(1) = false'
  ##  and we define 'IsPrimeInt( -<n> ) = IsPrimeInt( <n> )'.*/
  def IsPrimeInt(input : Int) : Boolean =
  {
    val n = abs(input)
    if (n < 2)
      false
    else
      if (Primes_low contains n)
        true
      else
        if (Primes_high contains n)
          true
        else
          false   //2DO - This relies on the list - check manually for values > 1000 like in gap
  }
/*#############################################################################
##
#F DivisorPairs(x)
#F   returns a list of factorizations of x in two factors,
#F   excluding [1,x] and [x,1]. Return type is
#F   a list of lists (each with 2 integers).
#F*/
  def DivisorPairs(n: Int) =  {    (2 to Math.sqrt(n).toInt ).filter(n%_== 0).flatMap(x=>(if(n/x == x) List(x) else List(n/x,x)) ).toList.sortWith(_>_).map(x=>List(n/x,x))  }

/*#############################################################################
  ##
  #F DivisorPairsRP(x)
  #F   returns a list of factorizations of x in two relatively
  #F   prime factors, excluding [1,x] and [x,1]. Return type is
  #F   a list of lists (each with 2 integers).
  #F*/
  def DivisorPairsRP(n: Int) = DivisorPairs(n).filter(pair => Gcd(pair(0),pair(1)) == 1)

  /*#############################################################################
  ##
  #F Gcd(x,y)
  #F
  #F*/

  def Gcd[A](x: A, y: A)(implicit integral: Integral[A]) : A =
  {
    val t = scala.math.BigInt(integral.toLong(x))
    val res = t.gcd(scala.math.BigInt(integral.toLong(y)))
    x match {
      case _:Int => res.toInt.asInstanceOf[A]
      case _:Long => res.toLong.asInstanceOf[A]
      case _:Short => res.toShort.asInstanceOf[A]
    }
  }

  /*#############################################################################
  ##
  #F rootsOfUnity(n:Int)
  #F
  #F*/

  //def rootsOfUnity(n:Int)=for(k <- 0 until n) yield Complex.fromPolar(1.0, 2*math.Pi*k/n)
  //def rootsOfUnity(n:Int)=for(k <- 0 until n) yield new Complex(cos(2*math.Pi*k/n),sin(2*math.Pi*k/n))//Complex.fromPolar(1.0, 2*math.Pi*k/n)
  def rootsOfUnity(n:Int, p: Int) : scala.collection.immutable.IndexedSeq[Complex] = {    //TODO - find short form for return value
     for(k <- 0 until n
       if ( //this if checks if x^t becomes 1 before n==t, this is e.g. the case for 2nd root of unity of 4 where it becomes 1 at x^2
          (for (t <- 2 until n-1
                  if (cos(2*math.Pi*k*t/n) == 1)
               )yield 1
          ).isEmpty
       )
     )
    yield new Complex(cos(2*math.Pi*k * p/n),-sin(2*math.Pi*k * p/n))


/*    val r = 2*k * p/n
    if (r > math.pi)
      normalize()

    cos(Pi*r)*/


  }
  def rootsOfUnity(n:Int) : scala.collection.immutable.IndexedSeq[Complex] = { rootsOfUnity(n,1) }

  /*#############################################################################
  ##
  #F firstrootOfUnity
  #F
  #F This takes simply the last rootOfUnity provided by rootsOfUnity which is the first in clockwise direction
  #F
  #F This is how FFTW defines first. GAP uses the first counterclockwise
  #F*/
  def firstrootOfUnity(n:Int)= rootsOfUnity(n).last
  def firstrootOfUnity(n:Int,p:Int)=
  {
    val tmp = rootsOfUnity(n,p)
    rootsOfUnity(n,p).last
  }
  //def firstrootOfUnity[T:NumericRep](n:Int,p:Int)=  ComplexDataType.E[T](n,p)



/*#############################################################################
  ##
  #F  PrimitiveRootMod( <m> ) . . . . . . . .  primitive root modulo an integer
  ##
  ##  'PrimitiveRootMod' returns the smallest primitive root modulo the integer
  ##  <m> and 'false' if no such primitive root exists.  If the optional second
  ##  integer argument <start> is given 'PrimitiveRootMod' returns the smallest
  ##  primitive root that is strictly larger than <start>.
  ##*/
  //val PrimitivRoots = List(0, 0, 1, 2, 3, 2, 5, 3, 0, 2, 3, 2, 0, 2, 3, 0, 0, 3, 5, 2, 0, 0, 7, 5, 0, 2, 7, 2, 0, 2, 0, 3, 0, 0, 3, 0, 0, 2, 3, 0, 0, 6, 0, 3, 0, 0, 5, 5, 0, 3, 3, 0, 0, 2, 5, 0, 0, 0, 3, 2, 0, 2, 3, 0, 0, 0, 0, 2, 0, 0, 0, 7, 0, 5, 5, 0, 0, 0, 0, 3, 0, 2, 7, 2, 0, 0, 3, 0, 0, 3, 0, 0, 0, 0, 5, 0, 0, 5, 3, 0, 0)
  val PrimitivRoots = List(0, 0, 1, 2, 3, 2, 5, 3, 0, 2, 3, 2, 0, 2, 3, 0, 0, 3, 5, 2, 0, 0, 7, 5, 0, 2, 7, 2, 0, 2, 0, 3, 0, 0, 3, 0, 0, 2, 3, 0, 0, 6, 0, 3, 0, 0, 5, 5, 0, 3, 3, 0, 0, 2, 5, 0, 0, 0, 3, 2, 0, 2, 3, 0, 0, 0, 0, 2, 0, 0, 0, 7, 0, 5, 5, 0, 0, 0, 0, 3, 0, 2, 7, 2, 0, 0, 3, 0, 0, 3, 0, 0, 0, 0, 5, 0, 0, 5, 3, 0, 0, 2, 0, 5, 0, 0, 3, 2, 0, 6, 0, 0, 0, 3, 0, 0, 0, 0, 11, 0, 0, 2,  7, 0, 0, 2, 0, 3, 0, 0, 0, 2, 0, 0, 7, 0, 0, 3, 0, 2, 0, 0, 7, 0, 0, 0, 5, 0, 0, 2, 0, 6, 0, 0, 0, 0, 0, 5, 3, 0, 0, 0, 5, 2, 0, 0, 5, 5, 0, 2, 0, 0, 0, 2, 0, 0, 0, 0, 3, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 0, 5, 5, 0, 0, 2, 0, 3, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 0, 2, 0, 0, 5, 0, 0, 0, 11, 0, 0, 0, 0, 3, 0, 0, 3, 2, 0, 6, 0,  0, 0, 3, 0, 0, 0, 0, 0, 7, 0, 7, 7, 2, 0, 0, 0, 0, 0, 0, 3, 6, 0, 0, 3, 0, 0, 3, 0, 0, 0, 0, 17, 5, 0, 0, 0, 0, 0, 2, 0, 6, 0, 0, 3, 0, 0, 5, 3, 0, 0, 3, 0, 3, 0, 0, 0, 0, 0, 3, 0, 0, 0, 2, 0, 0, 0, 0, 3, 0, 0, 0, 7, 0, 0, 0, 0, 5, 0, 0, 0, 17, 0, 10, 5, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 3, 0, 0, 5, 0,  0, 10, 7, 0, 0, 0, 0, 3, 0, 0, 3, 2, 0, 2, 0, 0, 0, 3, 0, 0, 0, 0, 7, 7, 0, 2, 21, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 19, 5, 0, 0, 5, 0, 0, 2, 0, 0, 0, 0, 3, 0, 0, 5, 3, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 5, 0, 0, 0, 0, 0, 15,  0, 0, 0, 2, 0, 0, 3, 0, 0, 3, 0, 0, 0, 0, 5, 0, 0, 13, 7, 0, 0, 2, 0, 3, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 13, 0, 0, 7, 0, 0, 0, 5, 3, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 11, 5, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 3, 0, 2, 0, 0, 5, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 2, 15, 0,  0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 5, 0, 0, 2, 0, 0, 0, 0, 3, 2, 0, 0, 3, 0, 0, 3, 0, 3, 0, 0, 0, 0, 0, 5, 3, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 7, 0, 7, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 2, 5, 0, 0, 3, 0, 2, 0, 0, 17, 0, 0, 2, 15, 0, 0, 0, 0, 3, 0, 0, 3, 0, 0, 0, 0, 0, 0, 3, 0, 11, 0, 0, 0, 5,  0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 15, 0, 0, 2, 0, 0, 0, 0, 0, 5, 0, 0, 3, 0, 0, 0, 0, 3, 0, 0, 5, 0, 0, 0, 7, 0, 0, 2, 0, 0, 0, 0, 3, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 7, 11, 0, 0, 3, 0, 0, 0, 0, 5, 0, 2, 0, 0, 0, 6, 11, 0, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 5, 0, 0, 0,  0, 3, 0, 0, 0, 0, 0, 2, 3, 0, 0, 6, 0, 0, 0, 0, 5, 0, 0, 11, 0, 0, 0, 2, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 5, 0, 0, 2, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 3, 0, 3, 0, 0, 0, 0, 0, 0, 21, 0, 0, 2, 0, 3, 0, 0, 0, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 11, 11, 0, 2, 23, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 2, 0, 0, 0, 3, 0, 2, 0, 0, 7, 5, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 15, 0, 0, 3, 0, 2, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 17, 0, 0, 13, 0, 0, 0, 0, 7, 0, 0, 3, 0, 0, 0, 3, 0, 0, 3, 0, 0, 0, 0, 5, 0, 0, 5, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0,  3, 0, 0, 0, 0, 13, 0, 0, 3, 0, 0, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 3, 0, 0, 3, 0, 0, 0, 0, 7, 5, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 7, 7, 0, 0 )
  def PrimitiveRoot(m: Int): Int =
  {
    val start = 1 //in case we later want to use a bigger PrimitivRoot
    return PrimitivRoots(m) //2DO: in case of DFT Radar the lookup is fine - not sure if we need to be able to handle all sizes

    //handle trivial cases
    if (m == 2 && start == 1)
       return 1
    if (m  == 2)
       return -1
    if (m == 4 && start <= 3)
      return 3
    if (mathmod(m,4) == 0)
      return -1


    def handle_even (m : Int) : (Int,Int) = //handle even numbers
      if (mathmod(m,2)== 0)
        return (2,m/2)
      else
        return (1,m)

    val (mm,m1) = handle_even(m)

    //check that m1 is a prime power otherwise no primitive root exists
    val p = SmallestRootInt(m1)
    if (! IsPrimeInt(p) )
      return -1

    //run through all the candidates for a primitive root
    var root = start + 1
    //TODO: Georg - this is not finished yet - still using lookup for the time being
    //while (root <= )
    return -1
  }

  def SmallestRootInt (n: Int) : Int =
  {
    if (n == 0) return 0

    //check the argument
    val (n1,k,s) =
       if (n > 0)
         (n,2,1)
       else
         (-n,3,-1)

    //exclude small divisors, and thereby large exponents

    val p =
      if (mathmod(n1,2) == 0)
        2
      else
      {
        var pt = 3
        while (pt < 100 && mathmod(n1,pt) != 0)
          pt = pt + 2
        pt
      }

    val l = (Math.log(p)/Math.log(n1)).toInt

    //loop over the posiible prime divisors of exponents
    //use Euler's criterion to cast out impossible ones

    var k1 = k
    var q = 0
    var r1 = 0
    var l1 = l
    var n2 = n1
    var r = 0
    while (k1 <= l1)
    {
      q = 2*k1+1
      while (IsPrimeInt(q))
        q = q+2*k1
      if (PowerModInt(n2,((q-1.0)/k1).toInt,q) <= 1)
      {
        r = Math.pow(n2, 1.0 / k1).toInt
        if (Math.pow(r,k1) == n2)
        {
          n2 = r
          l1 = (l1/k).toInt
        }
        else
          k1 = NextPrime(k1)
      }
      else
        k1 = NextPrime(k1)
    }

    return s * n2

  }

  def NextPrime (n: Int) : Int =
  {
    val combine = Primes_low ::: Primes_high
    val bigger = combine.filter(_ > n)
    bigger.head
  }


//PowerModInt(<r>,<e>,<m>)  . . . . . . power of one integer modulo another
  def PowerModInt (r: Int, e: Int, m: Int): Int =
  {
    //reduce r initially
    val r1 = mathmod(r,m)

    //handle special case
    if (e == 0) return 1

    if (e < 0)
      assert(false,"negativ version not yet implemented")

    var e1 = e
    var pow = 1

    var f = Math.pow(2,( (log(e)/log(2) ).toInt + 1 ))

    while (1 < f)
    {
      pow = mathmod(pow*pow,m)
      f = (f/2).toInt
      if (f <= e1){
        pow = mathmod((pow*r1),m)
        e1 = e1 - f.toInt
      }
    }
    return pow
  }




/*#############################################################################
  #F RR(<N>,<phi>,<g>)
  ##    Rader permutation: i -> {    0,     if i = 0 }
  ##                            { RM(i-1),  if i > 0 }
  ##    <phi> and <g> are parameters to RM
  ##
#F*/

  def IsWindows() =
  {
    val os = System.getProperty("os.name")
    val answer = os.startsWith("Windows")
    answer
  }

  val Primes_low =  List(   2,  3,  5,  7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53,
    59, 61, 67, 71, 73, 79, 83, 89, 97,101,103,107,109,113,127,131,137,139,
    149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,
    241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,
    353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,
    461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,
    587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,
    691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,
    823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,
    947,953,967,971,977,983,991,997 )
  val Primes_high = List(10047871, 10567201, 10746341, 12112549, 12128131, 12207031, 12323587,
    12553493, 12865927, 13097927, 13264529, 13473433, 13821503, 13960201,
    14092193, 14597959, 15216601, 15790321, 16018507, 18837001, 20381027,
    20394401, 20515111, 20515909, 21207101, 21523361, 22253377, 22366891,
    22996651, 23850061, 25781083, 26295457, 28325071, 28878847, 29010221,
    29247661, 29423041, 29866451, 32234893, 32508061, 36855109, 41540861,
    42521761, 43249589, 44975113, 47392381, 47763361, 48544121, 48912491,
    49105547, 49892851, 51457561, 55527473, 56409643, 56737873, 59302051,
    59361349, 59583967, 60816001, 62020897, 65628751, 69566521, 75068993,
    76066181, 85280581, 93507247, 96656723, 97685839,
    106431697, 107367629, 109688713, 110211473, 112901153, 119782433, 127540261,
    134818753, 134927809, 136151713, 147300841, 160465489, 164511353, 177237331,
    183794551, 184481113, 190295821, 190771747, 193707721, 195019441, 202029703,
    206244761, 212601841, 212885833, 228511817, 231769777, 234750601, 272010961,
    283763713, 297315901, 305175781, 308761441, 319020217, 359390389, 407865361,
    420778751, 424256201, 432853009, 457315063, 466344409, 510810301, 515717329,
    527093491, 529510939, 536903681, 540701761, 550413361, 603926681, 616318177,
    632133361, 715827883, 724487149, 745988807, 815702161, 834019001, 852133201,
    857643277, 879399649,
    1001523179, 1036745531, 1065264019, 1106131489, 1169382127, 1390636259,
    1503418321, 1527007411, 1636258751, 1644512641, 1743831169, 1824179209,
    1824726041, 1826934301, 1866013003, 1990415149, 2127431041, 2147483647)


  def NormalizeRational[A](x: A, y: A)(implicit integral: Integral[A]) : (A, A) = {
    val gcd = Gcd(x, y)
    (integral.quot(x, gcd), integral.quot(y, gcd))
  }

  def normalize_2pi_shift (xin: Double, yin: Double): (Double,Double) = {
    var (x, y) = NormalizeRational(Math.round(xin), Math.round(yin))
    if ( (x / y) < 0 ) {
      val t:Long = Math.ceil( x.toDouble / y.toDouble / (-2.0) ).toLong
      x = x + 2 * t * y
    } else {
      val t = (Math.floor( (x.toDouble - 2 * y.toDouble) / y.toDouble / 2.0 ) + 1 ).toLong;
      x = x - 2 * y * t;
    }
    val (xp, yp) = NormalizeRational(x,y)
    (xp.toDouble, yp.toDouble)
  }

  def normalize_pi_over2_shift (xin: Double, yin: Double): (Double,Double) = {
    val (x, y) = (Math.round(xin), Math.round(yin))
    val (xp, yp) = NormalizeRational(2 * x - y, 2 * y)
    (xp.toDouble, yp.toDouble)
  }

  def normalize_pi_over2_reflection(xin: Double, yin: Double): (Double,Double) = {
    val (x, y) = (Math.round(xin), Math.round(yin))
    val (xp, yp) = NormalizeRational(y - 2 * x, 2 * y)
    (xp.toDouble, yp.toDouble)
  }

  def normalize_trig (sign: Int, trig: String, x: Double, y: Double): (Int, String, Double, Double, Double) = {
    // normalization in 2Pi, achieving: 0 <= xn / yn <= 2
    var (xn,yn) = normalize_2pi_shift(x,y)
    if ( xn > yn ) {
      trig match {
        case "sin" => normalize_trig ( sign * (-1), "sin", xn - yn, yn )
        case "cos" => normalize_trig ( sign * (-1), "cos", xn - yn, yn )
      }
    } else if (xn == yn) {
      trig match {
        case "sin" => ( sign, "sin", xn, yn, sign * (+0.0) )
        case "cos" => ( sign, "cos", xn, yn, sign * (-1.0) )
      }
    } else {
      if ( xn > yn/2 ) {
        // normalization in Pi, achieving 0 <= xn / yn <= 1/2
        val (xp, yp) = normalize_pi_over2_shift(xn, yn)
        trig match {
          case "sin" => normalize_trig ( sign * (+1), "cos", xp, yp )
          case "cos" => normalize_trig ( sign * (-1), "sin", xp, yp )
        }
      } else if ( xn == yn / 2 ) {
        trig match {
          case "sin" => ( sign, "sin", xn, yn, sign * (+1.0) )
          case "cos" => ( sign, "cos", xn, yn, sign * (+0.0) )
        }
      } else {
        // now reflect in Pi / 2, and make sure that 0 <= xn / yn <= 1/4
        if ( xn > yn / 4 ) {
          val (xp, yp) = normalize_pi_over2_reflection(xn, yn)
          trig match {
            case "sin" => ( sign, "cos", xp, yp, Double.MaxValue )
            case "cos" => ( sign, "sin", xp, yp, Double.MaxValue )
          }
        } else if ( xn == yn / 4) {
          ( sign, "cos", 1.0, 4.0, Double.MaxValue )
        } else {
          if ( xn == 0.0 ) {
            trig match {
              case "sin" => ( sign, "sin", xn, yn, sign * (+0.0) )
              case "cos" => ( sign, "cos", xn, yn, sign * (+1.0) )
            }
          } else {
            trig match {
              case "sin" => ( sign, "sin", xn, yn, Double.MaxValue )
              case "cos" => ( sign, "cos", xn, yn, Double.MaxValue )
            }
          }
        }
      }
    }
  }

  trait TrigonometryOps [T] {
    /**
     * Performs normalization of sin and cos functions. The sin and cos function are in the form sin (rational * PI).
     * The function normalizes the value of the rational (which is x / y) to a value [0 .. PI / 4], and the result is
     * either a cos of a sin function. The function normalizes both staged and non-staged version of sin and cos.
     *
     * @param f Starting function. Either "sin" or "cos"
     * @param x Dividend component of the rational number
     * @param y Divisor component of the rational number
     * @return  Normalized sin or cos function (staged or non-staged depends on T)
     */
    private def valueSinOrCos (f: String, x: Double, y: Double) : T = {
      val (sign, trig, xn, yn, value) = normalize_trig(1, f, x, y)
      if ( !value.equals(scala.Double.MaxValue) ) {
        fromDouble(value)
      } else {
        trig match {
          case "sin" => (xn, yn) match {
            case (1.0, 6.0) => fromDouble (sign * 0.5)
            case _ => fromDouble(sign * Math.sin(xn * Math.PI/yn))
          }
          case "cos" => fromDouble ( sign * Math.cos(xn * Math.PI/yn) )
        }
      }
    }

    def SinPi(x : Double, y: Double) : T = valueSinOrCos("sin", x, y)
    def CosPi(x : Double, y: Double) : T = valueSinOrCos("cos", x, y)

    def fromDouble(x: Double)  : T
  }


}





