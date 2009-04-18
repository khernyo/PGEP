package pgep.GeneticOperators.Mutators

class Inversion(inversionProbability: Double, maxInversionSectorLen: Int, selection: GenotypeSelection) extends Mutator(selection) {
  require(inversionProbability >= 0)
  require(inversionProbability <= 1)
  require(maxInversionSectorLen > 0)
  
  protected[Mutators] override def apply(gt: Genotype) {
    for (gene <- gt.genes)
      if (random.nextDouble < inversionProbability) {
        val invLen = random.nextInt(maxInversionSectorLen + 1)
        val invStart = random.nextInt(gt.gp.headLen - invLen)
        val invEnd = invStart + invLen - 1
        
        for (j <- (0 until invLen / 2))
          for (tpe <- gene._k_expression.keys) {
            val tmp = gene._k_expression(tpe)(invStart + j)
            gene._k_expression(tpe)(invStart + j) = gene._k_expression(tpe)(invEnd - j)
            gene._k_expression(tpe)(invEnd - j) = tmp
          }
      }
  }
}
