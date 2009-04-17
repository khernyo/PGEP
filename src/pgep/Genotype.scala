package pgep

object Genotype {
  def apply(gp: GenotypeParameters) = {
    val genes: Seq[Gene] = (0 until gp.nrGenes) map (i => Gene(gp.geneParameters(i)))
    
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
      for (i <- (geneStart + 1) to geneEnd)
        Gene.copySymbols(src.genes(i), dst.genes(i), 0, 0, srcGeneLen)

      Gene.copySymbols(src.genes(geneEnd - 1), dst.genes(geneEnd - 1), 0, 0, symbolEnd)
    }
  }
}

class Genotype(val gp: GenotypeParameters,
               val genes: Seq[Gene]) {
  
  private var _matingProbability: Double = _
  def matingProbability = _matingProbability
  def matingProbability_=(p: Double) = {
    assert(p >= -0.00000000001 || p.isNaN || p.isInfinity)
    _matingProbability = if (p < 0 || p.isNaN || p.isInfinity) 0 else p
  }
  
  private var _fitness: Double = _
  def fitness = _fitness
  def fitness_=(f: Double) = _fitness = f
  
  def randomize(): Unit = randomize(() => true,() => true,() => true)
  def randomize(headp: () => Boolean, tailp: () => Boolean, constp: () => Boolean) {
    genes foreach {_.randomize(gp.selector_fvc, gp.selector_vc, headp, tailp, constp)}
  }
  
  def apply(variables: Map[String, Any]):List[Any] = {
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
  
  def meanSquaredError(errorFn: (Any, Any) => Double, variableCases: List[Map[String, Any]], expectedValues: List[List[Any]],
                        maxInvalidResults: Int) = {
    val errFn = Function.tupled(errorFn)
    val sumOfErrorSquares = (expected: List[Any], actual: List[Any]) => (expected zip actual) map errFn map (x => x * x) reduceLeft (_ + _)
    val invalid = (x: Double) => x.isNaN || x.isInfinity
    
    val results = variableCases map (this(_))
    val ses = (expectedValues zip results) map Function.tupled(sumOfErrorSquares)
    val nrInvalid = ses filter invalid length;
    
    if (nrInvalid > maxInvalidResults)
      Double.MaxValue
    else
      (ses remove invalid reduceLeft (_ + _)) / (variableCases.length - nrInvalid)
  }
  
  def cloneConsts() {
    for (gene <- genes)
      gene.cloneConsts()
  }
}

