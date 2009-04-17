package pgep

abstract class MutationBase(selection: GenotypeSelection) extends GeneticOperator(1, 1, selection){
  def apply(genotypes: Array[Genotype]) {
    for (sel <- selection.select(genotypes)) {
	  assert(sel.length == 1)
	  apply(genotypes(sel.head))
	}
  }
  
  protected def apply(gt: Genotype)
}
