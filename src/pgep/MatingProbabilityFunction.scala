package pgep

abstract class MatingProbabilityFunction {
  protected val random = RNGProvider()
  
  protected def calculateMatingProbability(min: Double, max: Double, median: Double, fitness: Double): Double
  
  def setMatingProbability(genotypes: List[Genotype]) = {
    val (min, max, median) = {
      val valid = (d: Double) => !(d.isNaN || d.isInfinity)
      val sorted = genotypes map (_.fitness) filter valid sort (_ < _)
      (sorted.head, sorted.last, sorted(sorted.length / 2))
    }

    genotypes foreach (gt => gt.matingProbability = calculateMatingProbability(min, max, median, gt.fitness))    

    val totalMatingProbability =  genotypes map (_.matingProbability) reduceLeft (_ + _)
    totalMatingProbability
  }
}
