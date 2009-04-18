package pgep.GeneticOperators.Mutators

class PartialTransposition(transpositionProbability: Double, maxTransposonLen: Int, selection: GenotypeSelection) extends Mutator(selection) {
  require(maxTransposonLen > 0)
  require(transpositionProbability >= 0)
  require(transpositionProbability <= 1)
  
  protected[Mutators] override def apply(gt: Genotype) {
    for (srcGene <- gt.genes)
      if (random.nextDouble() < transpositionProbability) {
        val trLen = random.nextInt(maxTransposonLen) + 1
        val trSrcStart = random.nextInt(gt.gp.headLen + gt.gp.tailLen - trLen)
        val dstGene = gt.genes(random.nextInt(gt.gp.nrGenes))
        val trDstStart = random.nextInt(gt.gp.headLen - trLen)
        
        copyTransposon(srcGene, dstGene, trSrcStart, trDstStart, trLen, gt.gp.headLen)
      }
  }
  
  protected def copyTransposon(src: Gene, dst: Gene, srcStart: Int, dstStart: Int, trLen: Int, headLen: Int) {
    Gene.copySymbols(dst, dst, dstStart, dstStart + trLen, headLen - (dstStart + trLen))
    Gene.copySymbols(src, dst, srcStart, dstStart, trLen)
  }
}
