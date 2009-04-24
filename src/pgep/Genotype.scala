package pgep

import List.map2

object Genotype {
  def apply(gp: GenotypeParameters) = {
    val genes = (0 until gp.nrGenes) map (i => Gene(gp.geneParameters(i))) toArray;    
    new Genotype(gp, genes)
  }
  
  def copyLinearStructure(src: Genotype, dst: Genotype, geneStart: Int, symbolStart: Int, geneEnd: Int, symbolEnd: Int) {
    assert(geneStart < geneEnd)
    assert(geneEnd - geneStart > 1 || symbolStart < symbolEnd)
    assert(geneStart >= 0)
    assert(geneEnd <= src.gp.nrGenes)
    assert(symbolStart >= 0)
    assert(symbolStart < src.gp.geneLen)
    assert(symbolEnd > 0)
    assert(symbolEnd <= src.gp.geneLen)
    
    val srcGeneLen = src.gp.geneLen
    val inOneGene = geneEnd - geneStart == 1
    
    Gene.copySymbols(src.genes(geneStart), dst.genes(geneStart), symbolStart, symbolStart, (if (inOneGene) symbolEnd else srcGeneLen) - symbolStart)
    
    if (!inOneGene) {
      for (i <- (geneStart + 1) until (geneEnd - 1))
        Gene.copySymbols(src.genes(i), dst.genes(i), 0, 0, srcGeneLen)

      Gene.copySymbols(src.genes(geneEnd - 1), dst.genes(geneEnd - 1), 0, 0, symbolEnd)
    }
  }
  
  private[this] final val isInvalid = (x: Double) => x.isNaN || x.isInfinity
}

class Genotype private (val gp: GenotypeParameters,
                        val genes: Array[Gene]) {
  import Genotype.isInvalid  
  
  private var _matingProbability: Double = _
  def matingProbability = _matingProbability
  def matingProbability_=(p: Double) = {
    assert(p >= -0.00000000001 || isInvalid(p))
    _matingProbability = if (p < 0 || isInvalid(p)) 0 else p
  }
  
  private var _fitness: Double = _
  def fitness = _fitness
  def fitness_=(f: Double) = _fitness = f
  
  def randomize(): Unit = randomize(() => true,() => true,() => true)
  def randomize(headp: () => Boolean, tailp: () => Boolean, constp: () => Boolean) {
    genes foreach {_.randomize(gp.selector_fvc, gp.selector_vc, headp, tailp, constp)}
  }
  
  def apply(variables: Map[Symbol, Any]):List[Any] = {
    val geneValues = genes map (_(variables)) toList;
    if (gp.geneLinkingFunction == null)
      geneValues
    else
      List(gp.geneLinkingFunction(geneValues, 0))
  }
  
  def toExpressionString = {
    val geneValues = genes map (_.toExpressionString)
    if (gp.geneLinkingFunction != null)
      gp.geneLinkingFunction.toExpressionString(geneValues, 0)
    else
      "[" + geneValues.mkString(", ") + "]"
  }
  
  def meanSquaredError(errorFn: (Any, Any) => Double, variableCases: List[Map[Symbol, Any]], expectedValues: List[List[Any]],
                        maxInvalidResults: Int) = {
    val sum = (l: List[Double]) => l.reduceLeft (_ + _)
    val sumOfSquaredError = (expected: List[Any], actual: List[Any]) => sum(map2(expected, actual)(errorFn).map(x => x * x))
    
    val results = variableCases map (this(_))
    val sse = map2(expectedValues, results)(sumOfSquaredError)
    val (invalid, valid) = sse partition isInvalid
    val nrInvalid = invalid length;
    
    if (nrInvalid > maxInvalidResults)
      Double.MaxValue
    else
      sum(valid) / valid.length
  }
  
  def cloneConsts() {
    for (gene <- genes)
      gene.cloneConsts()
  }
  
  override def clone() = new Genotype(gp, genes map (_.clone()))
}

