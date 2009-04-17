package pgep.GeneticOperators.Mutators

class Randomized(selection: GenotypeSelection) extends MutationBase(selection) {
  protected[Mutators] override def apply(gt: Genotype) {
    gt.randomize()
  } 
}
