package pgep

class Genotype(val gp: GenotypeParameters) {
  val genes: Seq[Gene] = (0 until gp.nrGenes) map (i => new Gene(gp.geneParameters(i)))
  
  protected var _matingProbability: Double = _
  val matingProbability = _matingProbability
  def matingProbability_=(p: Double) = {
    assert(p >= -0.00000000001 || p.isNaN || p.isInfinity)
    _matingProbability = if (p < 0 || p.isNaN || p.isInfinity) 0 else p
  }
  
  protected var _fitness: Double = _
  val fitness = _fitness
  def fitness_=(f: Double) = _fitness = f
  
  def randomize(): Unit = randomize(() => true,() => true,() => true)
  def randomize(headp: () => Boolean, tailp: () => Boolean, constp: () => Boolean) {
    genes foreach {_.randomize(gp.selector_fvc, gp.selector_vc, headp, tailp, constp)}
  }
  
  def apply(variables: Map[String, Any]) = {
    val geneValues = genes map (_(variables)) toArray;
    if (gp.geneLinkingFunction == null)
      geneValues
    else
      List(gp.geneLinkingFunction(geneValues, 0))
  }
}

