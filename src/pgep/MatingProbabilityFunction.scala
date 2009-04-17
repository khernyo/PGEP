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

class DirectFitnessToProbability extends MatingProbabilityFunction {
  protected override def calculateMatingProbability(min: Double, max: Double, median: Double, fitness: Double) = min + max - fitness
}

class SogartarParabolicMappingFitnessToProbability extends MatingProbabilityFunction {
  protected override def calculateMatingProbability(min: Double, max: Double, median: Double, fitness: Double) =
    if (min == max) 1
    else if (median <= fitness) 0.5 - Math.sqrt((fitness - median) / (4 * (max - median)))
      	 else 0.5 + Math.sqrt((median - fitness) / (4 * (median - min)))
}

class SogartarEllipticMappingFitnessToProbability extends MatingProbabilityFunction {
  protected override def calculateMatingProbability(min: Double, max: Double, median: Double, fitness: Double) =
    if (min == max) 1
    else if (median <= fitness) 0.5 - Math.sqrt((fitness - median) * (2 * max - median - fitness)) / (2 * (max - median))
    	 else 0.5 + Math.sqrt((median - fitness) * (median - 2 * min + fitness)) / (2 * (median - min))
}

class SogartarSemiEllipticSemiParabolic extends MatingProbabilityFunction {
  protected override def calculateMatingProbability(min: Double, max: Double, median: Double, fitness: Double) =
	if (min == max) 1
	else if (median <= fitness) (1 - Math.sqrt((fitness - median) * (2 * max - median - fitness)) / (2 * (max - median)) -
                                 Math.sqrt((fitness - median) / (4 * (max - median)))) / 2
         else (1 + Math.sqrt((median - fitness) * (median - 2 * min + fitness)) / (2 * (median - min)) +
               Math.sqrt((median - fitness) / (4 * (median - min)))) / 2
}