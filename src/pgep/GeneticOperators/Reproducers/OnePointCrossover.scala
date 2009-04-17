package pgep.GeneticOperators.Reproducers

class OnePointCrossover(selection: GenotypeSelection) extends Reproduction(2, 1, selection) {
  protected[Reproducers] override def apply(src: Array[Genotype], selectionIndices: List[Int], dst: Array[Genotype], dstIdx: Int) = {
    val (src1Idx, src2Idx) = if (random.nextInt(2) == 0) (0, 1) else (1, 0)
    val src1 = src(selectionIndices(src1Idx))
    val src2 = src(selectionIndices(src2Idx))
    
    val geneLen = src1.gp.geneLen
    val nrGenes = src1.gp.nrGenes
    val splitPos = random.nextInt(nrGenes * geneLen - 2) + 1     // don't split at the very beginning or end
    val symbolPos = splitPos % geneLen
    val genePos = splitPos / geneLen

    val child = Genotype(src1.gp)
    Genotype.copyLinearStructure(src1, child, 0, 0, genePos + 1, symbolPos + 1)
    Genotype.copyLinearStructure(src2, child, genePos, symbolPos, nrGenes, geneLen)

    dst(dstIdx) = child
    dstIdx + 1
  }
}
