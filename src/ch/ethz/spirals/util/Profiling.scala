package ch.ethz.spirals.util

/**
 * Georg Ofenbeck
 First created:
 * Date: 04/09/13
 * Time: 10:51 
 */
class Measurment (val name : String, val starttime: Long, val startmemory: Long){
  var stoptime: Long = 0
  var stopmemory : Long = 0
  def stop() = {
    stoptime = System.currentTimeMillis()
    stopmemory = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
  }
}


case class Profiling
{
  var measurments : List[Measurment] = List()
  def start(name: String) = {
    val x = new Measurment(name,
      System.currentTimeMillis(),
      Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
    )
    measurments = List(x) ::: measurments
  }
  def stop() = measurments.head.stop()
}


