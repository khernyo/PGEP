package pgep.GeneticOperators

class Randomized(selection: GenotypeSelection) extends MutationBase(selection) {
  protected override def apply(gt: Genotype) {
    gt.randomize()
  } 
}
