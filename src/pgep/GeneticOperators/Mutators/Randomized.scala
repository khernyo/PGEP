package pgep.GeneticOperators.Mutators

class Randomized(selection: GenotypeSelector) extends Mutator(selection) {
  protected[Mutators] override def apply(gt: Genotype) {
    gt.randomize()
  } 
}
