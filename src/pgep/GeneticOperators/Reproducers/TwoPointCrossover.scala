package pgep.GeneticOperators.Reproducers

class TwoPointCrossover(selection: GenotypeSelector) extends Reproducer(2, 1, selection) {
  def this() = this(null)
  
  protected[Reproducers] override def apply(selected: List[Genotype]) = {
    val (src1Idx, src2Idx) = if (random.nextInt(2) == 0) (0, 1) else (1, 0)
    val src1 = selected(src1Idx)
    val src2 = selected(src2Idx)
    
    val geneLen = src1.gp.geneLen
    val nrGenes = src1.gp.nrGenes
    val sp1 = random.nextInt(geneLen * nrGenes - 2) + 1	// don't split at the very beginning or end
    val sp2 = random.nextInt(geneLen * nrGenes - 2) + 1
    val splitPos1 = sp1 min sp2
    val splitPos2 = sp1 max sp2
    
    val symbolPos1 = splitPos1 % geneLen
    val genePos1 = splitPos1 / geneLen
    val symbolPos2 = splitPos2 % geneLen
    val genePos2 = splitPos2 / geneLen
    
    val child = Genotype(src1.gp)
    Genotype.copyLinearStructure(src1, child, 0, 0, genePos1 + 1, symbolPos1 + 1)
    Genotype.copyLinearStructure(src2, child, genePos1, symbolPos1, genePos2 + 1, symbolPos2 + 1)
    Genotype.copyLinearStructure(src1, child, genePos2, symbolPos2, nrGenes, geneLen)
    
    List(child)
  }
}
