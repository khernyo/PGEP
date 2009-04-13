package pgep

import java.util.Date
import java.io.FileWriter
import scala.util.Random
import scala.collection.mutable.{HashMap,SynchronizedMap}

object RNGProvider {
  var seed: Int = (new Date().getTime & Integer.MAX_VALUE).asInstanceOf[Int]
  private val logFile = "rngseeds.txt"
  private val rngs = new HashMap[Thread, Random] with SynchronizedMap[Thread,Random]
  
  private def createRng() = {
    val f = new FileWriter(logFile, true)
    f.write("%s ThreadId:%s ThreadName:\"%s\" seed:%s\n" format (new Date(), currentThread.getId, currentThread.getName, seed))
    f.close
    
    val rng = new Random(seed)
    rngs(currentThread) = rng
    rng
  }

  def apply() = rngs.getOrElseUpdate(currentThread, createRng())
}
