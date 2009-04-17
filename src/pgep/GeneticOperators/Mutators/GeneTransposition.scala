package pgep.GeneticOperators.Mutators

class GeneTransposition(transpositionProbability: Double, maxTransposonLen: Int, selection: GenotypeSelection) extends MutationBase(selection) {
  protected val transposon = new Array[Gene](maxTransposonLen)

  protected[Mutators] override def apply(gt: Genotype) {      // FIXME ez itt bugos bazmeg
    if (random.nextDouble() < transpositionProbability) {
      val len = random.nextInt(maxTransposonLen) + 1
      val srcIdx = random.nextInt(gt.gp.nrGenes - len)
      val dstIdx = random.nextInt(gt.gp.nrGenes - len)
      
      var k = 0
      var j = srcIdx
      while (k < len) {
        transposon(k) = gt.genes(j)
        j += 1
        k += 1
      }
      
      j = gt.gp.nrGenes - len - 1
      k = gt.gp.nrGenes - 1
      while (dstIdx + len <= j) {
        gt.genes(k) = gt.genes(j)
        j -= 1
        k -= 1
      }
      
      j = dstIdx
      k = 0
      while (k < len) {
        gt.genes(j) = transposon(k)
        j += 1
        k += 1
      }
    }
  }
}
