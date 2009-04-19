package pgep

abstract class Selector[T] {
  def apply(): T
}

abstract class ProbabilityBasedSelector[T] extends Selector[T] {
  protected val random = RNGProvider()
  
  protected def constructorPreconditions(probabilities: Seq[Double], values: Seq[T], scale: Double) {
    assert(probabilities != null)
    assert(values != null)
    assert(!probabilities.isEmpty)
    assert(probabilities.length == values.length)
    assert(probabilities forall (_ >= 0))
    assert(probabilities.reduceLeft(_ + _) == scale)
  }
}

class ExactSelector[T](probabilities: Seq[Double], val values: Seq[T], scale: Double) extends ProbabilityBasedSelector[T] {
  def this(probabilities: Seq[Double], values: Seq[T]) = this(probabilities, values, probabilities reduceLeft (_ + _))

  protected val probabilityDistribution = {
    constructorPreconditions(probabilities, values, scale)
    val s = if (scale == 0.0) 1.0 else scale
    var sum: Double = 0
    val pdist = probabilities map (p => {sum += p / s; sum})
    constructorPostconditions(probabilities, pdist)
    pdist
  }
  
  protected def constructorPostconditions(probabilities: Seq[Double], pdist: Seq[Double]) {
    assert(probabilities.length == pdist.length)
    assert(pdist forall (d => d <= 1.00000000001 && d >= -0.00000000001))
    val pdistl = pdist.toList
    assert((pdistl zip pdistl.tail) forall {case (d1, d2) => d1 <= d2})
    assert(Math.abs(pdist.last - 1) < 0.00000000001 || pdist.last == 0.0)
  }
  
  override def apply() = {
    val rnd = random.nextDouble
    val idx = probabilityDistribution map (rnd < _) indexOf true
    if (idx < 0) values((rnd * values.length).toInt)
    else values(idx)
  }
}

class SampledSelector[T](probabilities: Seq[Double], val values: Seq[T], scale: Double, nrsamples: Int) extends ProbabilityBasedSelector[T] {
  def this(probabilities: Seq[Double], values: Seq[T], scale: Double) = this(probabilities, values, scale, 1000)
  def this(probabilities: Seq[Double], values: Seq[T]) = this(probabilities, values, probabilities reduceLeft (_ + _))
  
  protected val samples = {
    constructorPreconditions(probabilities, values, scale)
    val smpls = ((values.toList zip probabilities.toList) flatMap {case (value, prob) => (List.make(Math.floor(prob * nrsamples.toDouble).toInt, value))}).toArray
    constructorPostconditions(values, nrsamples, smpls)
    smpls
  }
  
  protected def constructorPostconditions(values: Seq[T], nrsamples: Int, samples: Array[T]) {
    assert(samples.length == nrsamples)
    assert(samples forall (values.contains(_)))
    assert(values forall (samples.contains(_)))
  }
  
  protected def constructorPreconditions(probabilities: Seq[Double], values: Seq[T], scale: Double, nrsamples: Int) {
    constructorPreconditions(probabilities, values, scale)
    assert(nrsamples > values.length)
  }
  
  override def apply() = {
    samples(random.nextInt(samples.length))
  }
}