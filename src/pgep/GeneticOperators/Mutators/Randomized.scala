package pgep.GeneticOperators.Mutators

class Randomized(selection: GenotypeSelection) extends Mutator(selection) {
  protected[Mutators] override def apply(gt: Genotype) {
    gt.randomize()
  } 
}
