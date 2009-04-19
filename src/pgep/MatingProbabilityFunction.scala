package pgep

abstract class MatingProbabilityFunction {
  protected val random = RNGProvider()
  
  protected def calculateMatingProbability(min: Double, max: Double, median: Double, fitness: Double): Double
  
  def setMatingProbability(genotypes: Seq[Genotype]) = {
    val valid = (d: Double) => !(d.isNaN || d.isInfinity)
    val sorted = genotypes.map(_.fitness).filter(valid).toList.sort(_ < _)
    
    val min = sorted.head
    val max = sorted.last
    val median = sorted(sorted.length / 2)

    genotypes foreach (gt => gt.matingProbability = calculateMatingProbability(min, max, median, gt.fitness))    

    genotypes map (_.matingProbability) reduceLeft (_ + _)
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