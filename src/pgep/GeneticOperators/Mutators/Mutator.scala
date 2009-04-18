package pgep.GeneticOperators.Mutators

abstract class Mutator(selection: GenotypeSelection) extends GeneticOperator(1, 1, selection){
  def apply(genotypes: Array[Genotype]) {
    for (sel <- selection.select(genotypes)) {
	  assert(sel.length == 1)
	  apply(genotypes(sel.head))
	}
  }
  
  protected[Mutators] def apply(gt: Genotype)
}
